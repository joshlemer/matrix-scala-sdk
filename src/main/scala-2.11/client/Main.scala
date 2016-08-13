package client

import akka.actor.ActorSystem
import akka.http.scaladsl._
import akka.http.scaladsl.client.RequestBuilding._
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpRequest, StatusCode, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import request._
import response._
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success}

package object client {
  implicit class AnyAsOptionExtension[T <: Any](val any: T) extends AnyVal {
    @inline final def ? = Option(any)
  }
}

object Main extends App {

  val serverUrl = "https://matrix.org"

  val client = new MatrixClient(serverUrl)

  implicit val ec = client.ec

//  client.r0.login.post("username", "password").andThen { case x => println(x); client.shutDown()}
  client.r0.login.post("username","password") flatMap { case loginResponse =>
    client.r0.sync.get(loginResponse.accessToken).andThen{
      case Success(syncResponse) =>
        ()
      case Failure(e) =>
        ()
    }.andThen { case x =>
      println(x)
      client.shutDown()
    }
  }
}

class MatrixClient(val serverUrl: String) {
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import MatrixJsonProtocol._

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val http = Http()
  val clientEndpoint = s"$serverUrl/_matrix/client"


  def single[T: JsonFormat](req: HttpRequest): Future[T] = {
    implicit val rf = rootFormat(eitherFormat[ErrorResponse, T](errorResFormat, implicitly[JsonFormat[T]]))
    http.singleRequest(req).flatMap(res => Unmarshal(res.entity).to[Either[ErrorResponse, T]]).flatMap {
      case Right(t) => Future.successful(t)
      case Left(e) => Future.failed(ErrorResponseException(e))
    }
  }

  def singleToStatus(req: HttpRequest): Future[StatusCode] = http.singleRequest(req).flatMap {
    case res if res.status.isSuccess() => Future.successful(res.status)
    case res => Unmarshal(res).to[ErrorResponse].flatMap(e => Future.failed(ErrorResponseException(e)))
  }

  object versions {
    def get() = single(Get(s"$clientEndpoint/versions"))(versionsFormat)
  }

  object r0 {
    private val versionEndpoint = s"$clientEndpoint/r0"
    object login {
      private val loginEndpoint = s"$versionEndpoint/login"
      def post(user: String, password: String) = single[LoginResponse](Post(loginEndpoint, loginEntity(user, password)))
    }
    object register {
      private val registerEndpoint = s"$versionEndpoint/register"
      def post(userKind: UserKind, userName: String, password: String, bindEmail: Boolean, authenticationData: AuthenticationData) =
        single[RegisterResponse](Post(registerEndpoint, registerEntity(userName, password, bindEmail, authenticationData)))

      object email {
        private val emailEndpoint = s"$versionEndpoint/email"
        object requestToken {
          val requestTokenEndpoint = s"$emailEndpoint/requestToken"
          def post(clientSecret: String, idServer: String, sendAttempt: Int, email: String) =
            singleToStatus(Post(requestTokenEndpoint))
        }
      }
    }
    object tokenRefresh {
      private val tokenRefreshEndpoint = s"$versionEndpoint/tokenrefresh"
      def post(refreshToken: String) = single[TokenRefreshResponse](Post(tokenRefreshEndpoint, tokenRefreshEntity(refreshToken)))
    }
    object logout {
      private val logoutEndpoint = s"$versionEndpoint/logout"
      def post() = http.singleRequest(Post(logoutEndpoint)).map(_.status)
    }
    object account {
      val accountEndpoint = s"$versionEndpoint/account"
      object password {
        object email {
          object requestToken {
            private val requestTokenEndpoint = s"$accountEndpoint/password/email/requestToken"
            def post(auth: AuthenticationData) = singleToStatus(Post(requestTokenEndpoint, auth))
          }
        }
      }
      object deactivate {
        private val deactivateEndpoint = s"$accountEndpoint/deactivate"
        def post(auth: AuthenticationData) = singleToStatus(Post(deactivateEndpoint))
      }

      object _3pid {
        private val _3pidEndpoint = s"$accountEndpoint/3pid"
        def get() = single[_3pidResponse](Get(_3pidEndpoint))(_3pidResFormat)
        def post(threePidCreds: ThreePidCredentials, bind: Boolean = false) = singleToStatus(Post(_3pidEndpoint, _3pidEntity(threePidCreds, bind)))
        object email {
          object requestToken {
            private val requestTokenEndpoint = s"${_3pidEndpoint}/email/requestToken"
            def post() = singleToStatus(Post(requestTokenEndpoint))
          }
        }
      }
    }
    case class user(userId: String) {
      object filter {
        val filterEndpoint = s"$versionEndpoint/user/$userId/filter"
        def post(
          eventFields: Option[List[String]],
          eventFormat: EventFormat,
          accountData: AccountData,
          room: RoomFilter,
          presence: Filter
          ) = ???
      }
      case class filter(filterId: String) {
        val filterEndpoint = s"$versionEndpoint/user/$userId/filter/$filterId"
        def get() = singleToStatus(Get(filterEndpoint))
      }
    }

    object sync {
      val syncEndpoint = s"$versionEndpoint/sync"
      import request.Presence
      def get(accessToken: String, filter: Option[String] = None, since: Option[String] = None, timeout: Int = 4000, fullState: Boolean = false, setPresence: Option[Presence] = None): Future[SyncResponse] = {

        val params = Map("full_state" -> fullState.toString, "access_token" -> accessToken) ++
          filter.map("filter" -> _) ++ since.map("since" -> _)


        single[SyncResponse](Get(Uri(syncEndpoint).withQuery(Query(params))))
      }
    }

    object createRoom {
      private val createRoomEndpoint = s"$versionEndpoint/createRoom"
      def post(
        preset: Preset,
        invite: Seq[String] = Seq.empty,
        /**If this is included, an m.room.name event will be sent into the room to indicate the name of the room. */
        name: Option[String] = None,
        visibility: Visibility = Visibility.Private,
        invite3pids: Seq[Invite3pid] = Seq.empty,
        topic: Option[String] = None,
        initialState: Seq[StateEvent] = Seq.empty,
        roomAliasName: Option[String] = None
        ) = {
        val req = createRoomRequest(preset, invite, name, visibility, invite3pids, topic, initialState, roomAliasName)
        single[RoomId](Post(createRoomEndpoint, req))(roomIdFormat)
      }
    }
  }

  def shutDown() = {
    http.shutdownAllConnectionPools()
    system.terminate()
  }
}

case class ErrorResponseException(errorResponse: ErrorResponse) extends Exception


object MatrixJsonProtocol extends DefaultJsonProtocol with ResponseFormats with RequestFormats {


  def loginEntity(user: String, password: String) =
    JsObject("user" -> user.toJson, "password" -> password.toJson, "type" -> "m.login.password".toJson, "medium" -> "email".toJson)

  def registerEntity(userName: String, password: String, bindEmail: Boolean, authenticationData: AuthenticationData) =
    JsObject("username" -> userName.toJson, "password" -> password.toJson, "bind_email" -> bindEmail.toJson,
    "auth" -> authenticationData.toJson)

  def tokenRefreshEntity(refreshToken: String) = JsObject("refresh_token" -> refreshToken.toJson)

  implicit lazy val threePidCredsFormat = jsonFormat(ThreePidCredentials, "client_secret", "id_server", "sid")
  def _3pidEntity(threePidCredentials: ThreePidCredentials, bind: Boolean) = JsObject("three_pid_creds" -> threePidCredentials.toJson, "bind" -> bind.toJson)
}
