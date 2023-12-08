//> using scala 3.3.1
//> using resourceDir .
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server:1.9.3
//> using dep com.softwaremill.sttp.tapir::tapir-json-circe:1.9.3
//> using dep com.github.pureconfig::pureconfig-core:0.17.4
//> using dep io.circe::circe-generic::0.14.6
//> using dep io.scalaland::chimney:0.8.3

// Here we define our Memo's domain models and services:
package domain {
  // domain model:
  case class Memo(name: String, content: String)

  // domain service:
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

// Here we define JSON entities and HTTP endpoints:
package endpoints {
  // API models:

  // { "content": "...", "name": "..." }
  case class MemoApi(content: String, name: String)
  // { "result": null } or
  // { "result": { "content": "...", "name": "..." } }
  case class FoundMemo(result: Option[MemoApi])

  // API endpoints logic:
  trait MemoController {
    def getMemo(passwd: Option[String], id: String): Either[String, FoundMemo]
    def setMemo(
        passwd: Option[String],
        id: String,
        memo: MemoApi
    ): Either[String, FoundMemo]
  }

  // Nice library for JSON serialization
  import io.circe.generic.auto.*
  // Nice library for defining endpoints with DSL
  // and then deciding how to run them.
  import sttp.tapir.*
  import sttp.tapir.json.circe.*
  import sttp.tapir.generic.auto.*

  // Definition of:
  //   GET /memos/{id}?passwd={passwd} -> [FoundMemo JSON]
  // routing
  val getMemo = endpoint.get
    .in("memos")
    .in(path[String]("id"))
    .in(query[Option[String]]("passwd"))
    .out(jsonBody[FoundMemo]) // <- type class derivation of JSON decoder!
    .errorOut(stringBody)

  // Definition of:
  //   PUT /memos/{id}?passwd={passwd} [MemoApi JSON] -> [FoundMemo JSON]
  // routing
  val setMemo = endpoint.put
    .in("memos")
    .in(path[String]("id"))
    .in(query[Option[String]]("passwd"))
    .in(jsonBody[MemoApi]) // <- type class derivation of JSON encoder!
    .out(jsonBody[FoundMemo]) // <- type class derivation of JSON decoder!
    .errorOut(stringBody)

  // Taking our endpoint definitions and turning into Netty server:
  import scala.concurrent.Future
  import sttp.tapir.server.netty.NettyFutureServerBinding

  def startServer(
      controller: MemoController
  ): Future[NettyFutureServerBinding] = {
    import sttp.tapir.server.netty.NettyFutureServer
    import scala.concurrent.ExecutionContext.Implicits.global

    NettyFutureServer()
      // wires GET /memos/... route to controller.getMemo
      .addEndpoint(getMemo.serverLogicPure { (id, passwd) =>
        controller.getMemo(passwd, id)
      })
      // wires PUT /memos/... route to controller.setMemo
      .addEndpoint(setMemo.serverLogicPure { (id, passwd, memo) =>
        controller.setMemo(passwd, id, memo)
      })
      // starts server on default ports
      .start()
  }
}

package app {

  // configs:
  case class MemoConfig(passwd: String)
  case class AppConfig(memo: MemoConfig)
  object AppConfig {

    def getOrThrow: AppConfig = {
      // Nice library for parsing HOCON (Typesafe Config) into case clases
      import pureconfig.*
      import pureconfig.generic.derivation.default.*
      // Type class derivation of our config's parsers
      given ConfigReader[AppConfig] = ConfigReader.derived[AppConfig]
      ConfigSource.default
        .load[AppConfig]
        .fold(e => throw Exception(e.prettyPrint()), a => a)
    }
  }

  // business logic using domain models:
  class MemoControllerImpl(config: MemoConfig, service: domain.MemoService)
      extends endpoints.MemoController {
    // Nice library for type mapping (like MapStruct/Dozer/Orca/JMapper/...)
    import io.scalaland.chimney.dsl.*

    def getMemo(
        passwd: Option[String],
        id: String
    ): Either[String, endpoints.FoundMemo] =
      if (passwd.contains(config.passwd)) {
        val memo = service.get(id)
        Right(
          // Type class derivation converting Option[Memo] -> Option[MemoApi]
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
        // Type class derivation converting MemoApi -> Memo
        service.set(id, memoApi.transformInto[domain.Memo])
        Right(endpoints.FoundMemo(Some(memoApi)))
      } else {
        Left("Wrong password")
      }
  }
}

// wiring everything together
@main def runExample: Unit = {
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
