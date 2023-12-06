//> using scala 3.3.1
//> using resourceDir .
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server:1.9.3
//> using dep com.softwaremill.sttp.tapir::tapir-json-circe:1.9.3
//> using dep com.github.pureconfig::pureconfig-core:0.17.4
//> using dep io.circe::circe-generic::0.14.6
//> using dep io.scalaland::chimney:0.8.3

package domain {
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

package endpoints {
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

  trait MemoController {
    def getMemo(passwd: Option[String], id: String): Either[String, FoundMemo]
    def setMemo(
        passwd: Option[String],
        id: String,
        memo: MemoApi
    ): Either[String, FoundMemo]
  }

  import scala.concurrent.Future
  import sttp.tapir.server.netty.NettyFutureServerBinding

  def startServer(
      controller: MemoController
  ): Future[NettyFutureServerBinding] = {
    import sttp.tapir.server.netty.NettyFutureServer
    import scala.concurrent.ExecutionContext.Implicits.global

    NettyFutureServer()
      .addEndpoint(getMemo.serverLogicPure { (id, passwd) =>
        controller.getMemo(passwd, id)
      })
      .addEndpoint(setMemo.serverLogicPure { (id, passwd, memo) =>
        controller.setMemo(passwd, id, memo)
      })
      .start()
  }
}

package app {

  case class MemoConfig(passwd: String)
  case class AppConfig(memo: MemoConfig)
  object AppConfig {

    def getOrThrow: AppConfig = {
      import pureconfig.*
      import pureconfig.generic.derivation.default.*
      given ConfigReader[AppConfig] = ConfigReader.derived[AppConfig]
      ConfigSource.default
        .load[AppConfig]
        .fold(e => throw Exception(e.prettyPrint()), a => a)
    }
  }

  class MemoControllerImpl(config: MemoConfig, service: domain.MemoService)
      extends endpoints.MemoController {
    import io.scalaland.chimney.dsl.*

    def getMemo(
        passwd: Option[String],
        id: String
    ): Either[String, endpoints.FoundMemo] =
      if (passwd.contains(config.passwd)) {
        val memo = service.get(id)
        Right(
          endpoints.FoundMemo(memo.transformInto[Option[endpoints.MemoApi]])
        )
      } else {
        Left("Wrong password")
      }

    def setMemo(
        passwd: Option[String],
        id: String,
        memoApi: endpoints.MemoApi
    ): Either[String, endpoints.FoundMemo] =
      if (passwd.contains(config.passwd)) {
        service.set(id, memoApi.transformInto[domain.Memo])
        Right(endpoints.FoundMemo(Some(memoApi)))
      } else {
        Left("Wrong password")
      }
  }
}

@main
def runExample: Unit = {
  val config = app.AppConfig.getOrThrow
  val service = domain.MemoService.inMemory
  val controller = new app.MemoControllerImpl(config.memo, service)
  val serverStartup = endpoints.startServer(controller)

  import scala.concurrent.Await
  import scala.concurrent.duration.Duration
  import scala.concurrent.ExecutionContext.Implicits.global

  val serverShutdown = serverStartup.flatMap { binding =>
    println(s"Config: $config")
    println(s"Bound to ${binding.hostName}:${binding.port}")
    println("Press Ctrl+D to shutdown")
    while (scala.io.StdIn.readLine() != null)
      () // block until the stream is closed
    println("Shutting down")
    binding.stop()
  }

  Await.result(serverShutdown, Duration.Inf)

  sys.exit(0)
}
