//> using scala 3.3.1
//> using resourceDir .
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server:1.9.3
//> using dep com.softwaremill.sttp.tapir::tapir-json-circe:1.9.3
//> using dep com.github.pureconfig::pureconfig-core:0.17.4
//> using dep io.circe::circe-generic::0.14.6
//> using dep io.scalaland::chimney:0.8.3


object Domain {
  case class Memo(name: String, content: String)

  trait MemoService {
    def get(id: String): Option[Memo]
    def set(id: String, value: Memo): Unit
  }
  object MemoService {
    def inMemory: MemoService =
      new MemoService {
        private val store =
          java.util.concurrent.atomic.AtomicReference(Map.empty[String, Memo])
        def get(id: String): Option[Memo] =
          store.get().get(id)
        def set(id: String, value: Memo): Unit =
          store.getAndUpdate(entries => entries.updated(id, value))
      }
  }
}

object Endpoints {
  case class MemoApi(content: String, name: String)
  case class FoundMemo(result: Option[MemoApi])

  import sttp.tapir.*
  import sttp.tapir.json.circe.*
  import sttp.tapir.generic.auto.*
  import io.circe.generic.auto.*

  // GET /memos/{id}?passwd={passwd}
  val getMemo = endpoint.get
    .in("memos")
    .in(path[String]("id"))
    .in(query[Option[String]]("passwd"))
    .out(jsonBody[FoundMemo])
    .errorOut(stringBody)

  // PUT /memos/{id}?passwd={passwd} [memo api payload]
  val setMemo = endpoint.put
    .in("memos")
    .in(path[String]("id"))
    .in(query[Option[String]]("passwd"))
    .in(jsonBody[MemoApi])
    .out(jsonBody[FoundMemo])
    .errorOut(stringBody)

  trait Controller {
    def getMemo(passwd: Option[String], id: String): Either[String, FoundMemo]
    def setMemo(
        passwd: Option[String],
        id: String,
        memo: MemoApi
    ): Either[String, FoundMemo]
  }

  def startServer(controller: Controller): Unit = {
    import sttp.tapir.server.netty.{NettyFutureServer, NettyFutureServerBinding}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration.Duration

    val binding = NettyFutureServer()
      .addEndpoint(getMemo.serverLogicPure { (id, passwd) =>
        controller.getMemo(passwd, id)
      })
      .addEndpoint(setMemo.serverLogicPure { (id, passwd, memo) =>
        controller.setMemo(passwd, id, memo)
      })
      .start()

    Await.result(
      binding.flatMap { binding =>
        println(s"Bound to ${binding.hostName}:${binding.port}")
        println("Press Ctrl+D to shutdown")
        while (scala.io.StdIn.readLine() != null)
          () // block until the stream is closed
        binding.stop()
      },
      Duration.Inf
    )
  }
}

object App {

  import pureconfig._
  import pureconfig.generic.derivation.default._

  case class MemoConfig(passwd: String) derives ConfigReader
  case class AppConfig(memo: MemoConfig) derives ConfigReader
  object AppConfig {

    def getOrThrow: AppConfig = {
      ConfigSource.default.load[AppConfig].fold(e => throw Exception(e.prettyPrint()), a => a)
    }
  }

  class MemoController(config: MemoConfig, service: Domain.MemoService) extends Endpoints.Controller {
    import io.scalaland.chimney.dsl.*

    def getMemo(passwd: Option[String], id: String): Either[String, Endpoints.FoundMemo] = 
      if (passwd.contains(config.passwd)) {
        val memo = service.get(id)
        Right(Endpoints.FoundMemo(memo.transformInto[Option[Endpoints.MemoApi]]))
      } else {
        Left("Wrong password")
      }

    def setMemo(
        passwd: Option[String],
        id: String,
        memo: Endpoints.MemoApi
    ): Either[String, Endpoints.FoundMemo] =
      if (passwd.contains(config.passwd)) {
        service.set(id, memo.transformInto[Domain.Memo])
        Right(Endpoints.FoundMemo(Some(memo)))
      } else {
        Left("Wrong password")
      }
  }
}

@main
def runExample: Unit = {
  val config = App.AppConfig.getOrThrow
  val service = Domain.MemoService.inMemory
  val controller = new App.MemoController(config.memo, service)
  Endpoints.startServer(controller)
  println("Server shut down")
  sys.exit(0)
}
