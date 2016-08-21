package client

import scala.concurrent.Future

import akka.actor.{ActorSystem, Terminated}
import akka.http.scaladsl._
import akka.http.scaladsl.client.RequestBuilding._
import akka.http.scaladsl.model.{HttpResponse, HttpRequest, StatusCode, Uri}, Uri.Query
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import spray.json._

import request._
import response._

package object client {
  implicit class AnyAsOptionExtension[T <: Any](val any: T) extends AnyVal {
    @inline final def ? = Option(any)
  }
}

class MatrixClient(val serverUrl: String)(implicit system: ActorSystem = ActorSystem()) {
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import MatrixJsonProtocol._

  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val http = Http()

  def single[T](req: HttpRequest)(implicit fmt: JsonFormat[T]): Future[T] = {
    implicit val rf = rootFormat(eitherFormat[ErrorResponse, T](errorResFormat, fmt))
    singleToResponse(req).flatMap(res => Unmarshal(res.entity).to[Either[ErrorResponse, T]]).flatMap {
      case Right(t) => Future.successful(t)
      case Left(e) => Future.failed(ErrorResponseException(e))
    }
  }
  
  def singleToResponse(req: HttpRequest): Future[HttpResponse] = http.singleRequest(req)

  def singleToStatus(req: HttpRequest): Future[StatusCode] = singleToResponse(req).flatMap {
    case res if res.status.isSuccess() => Future.successful(res.status)
    case res => Unmarshal(res).to[ErrorResponse].flatMap(e => Future.failed(ErrorResponseException(e)))
  }

  private implicit class QueryWithAccessToken(query: Query) {
    def withAccessToken(accessToken: String) = Query(query.toMap + ("access_token" -> accessToken))
  }
  private implicit class UriWithAccessToken(uri: Uri) {
    def withAccessToken(accessToken: String) = uri.withQuery(Query.Empty.withAccessToken(accessToken))
  }

  object versions {
    def get() = single(Get(s"$clientEndpoint/versions"))(versionsFormat)
  }

  private val clientEndpoint = s"$serverUrl/_matrix/client"
  private val versionEndpoint = s"$clientEndpoint/r0"

  val login: LoginSupport = new LoginSupport {
    private val loginEndpoint = s"$versionEndpoint/login"
    def post(user: String, password: String) = single[LoginResponse](Post(loginEndpoint, loginEntity(user, password)))
  }
  object register {
    private val registerEndpoint = s"$versionEndpoint/register"
    def post(userKind: UserKind = UserKind.User, userName: String, password: String, bindEmail: Boolean, authenticationData: AuthenticationData = AuthenticationData(None, "m.login.dummy")) = {
      val query = Query("kind" -> userKind.entryName)
      single[RegisterResponse](Post(Uri(registerEndpoint).withQuery(query), registerEntity(userName, password, bindEmail, authenticationData)))
    }

    val email = new RegisterEmailSupport {
      private val emailEndpoint = s"$versionEndpoint/email"
      val requestToken = new RequestTokenSupport {
        private val requestTokenEndpoint = s"$emailEndpoint/requestToken"
        def post(clientSecret: String, idServer: Option[String], sendAttempt: Int, email: String) = {
          val p = Post(requestTokenEndpoint, requestTokenEntity(clientSecret, idServer, sendAttempt.toString, email))
          singleToStatus(Post(requestTokenEndpoint, requestTokenEntity(clientSecret, idServer, sendAttempt.toString, email)))
        }
      }
    }
  }
  object tokenRefresh {
    private val tokenRefreshEndpoint = s"$versionEndpoint/tokenrefresh"
    def post(accessToken: AccessToken, refreshToken: String) =
      single[TokenRefreshResponse](Post(Uri(tokenRefreshEndpoint).withAccessToken(accessToken),
        singleStringValueFormat("refresh_token").write(refreshToken)))
  }
  object logout {
    private val logoutEndpoint = s"$versionEndpoint/logout"
    def post(accessToken: AccessToken) = singleToStatus(Post(Uri(logoutEndpoint).withAccessToken(accessToken)))
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
      def post(accessToken: AccessToken, auth: AuthenticationData) =
        singleToStatus(Post(Uri(deactivateEndpoint).withAccessToken(accessToken)))
    }

    object _3pid {
      private val _3pidEndpoint = s"$accountEndpoint/3pid"
      def get(accessToken: AccessToken) = single[_3pidResponse](Get(Uri(_3pidEndpoint).withAccessToken(accessToken)))(_3pidResFormat)
      def post(accessToken: AccessToken, threePidCreds: ThreePidCredentials, bind: Boolean = false) =
        singleToStatus(Post(Uri(_3pidEndpoint).withAccessToken(accessToken), _3pidEntity(threePidCreds, bind)))
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
//        def post(
//          accessToken: AccessToken,
//          eventFields: Option[List[String]],
//          eventFormat: EventFormat,
//          accountData: AccountData,
//          room: RoomFilter,
//          presence: Filter
//          ) = {
//          val req =
//          single[String](Post(Uri(filterEndpoint).withAccessToken(accessToken), filter))(singleStringValueFormat("filter_id"))
//        }
    }
    case class filter(filterId: String) {
      val filterEndpoint = s"$versionEndpoint/user/$userId/filter/$filterId"
      def get() = single[JsObject](Get(filterEndpoint))
    }
  }

  object sync {
    val syncEndpoint = s"$versionEndpoint/sync"
    import request.Presence
    def get(accessToken: String, filter: Option[String] = None, since: Option[String] = None, timeout: Int = 4000, fullState: Boolean = false, setPresence: Option[Presence] = None): Future[SyncResponse] = {

      val params = Map("full_state" -> fullState.toString) ++ filter.map("filter" -> _) ++ since.map("since" -> _)

      single[SyncResponse](Get(Uri(syncEndpoint).withQuery(Query(params).withAccessToken(accessToken))))
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
  case class rooms(roomId: RoomId) {
    val roomEndpoint = s"$versionEndpoint/rooms/$roomId"
    object join {
      def post(thirdPartySigned: Option[ThirdPartySigned] = None): Future[RoomId] = {
        val joinEndpoint = s"$roomEndpoint/join"
        val req = JsObject(thirdPartySigned.map("third_party_signed" -> _.toJson).toMap)
        single[RoomId](Post(Uri(joinEndpoint), req))(roomIdFormat)
      }
    }
    object state {
      val stateEndpoint = s"$roomEndpoint/state"
      def get(accessToken: String) = single[JsArray](Get(Uri(stateEndpoint).withAccessToken(accessToken)))
    }
    case class state(eventType: String) {
      val stateEndpoint = s"$roomEndpoint/state/$eventType"
      def get(): Future[String] = single[String](Get(stateEndpoint))(nameFormat)
      def put(stateKey: String, jsObject: JsObject) = single[String](Put(s"$stateEndpoint/$stateKey", jsObject))(singleStringValueFormat("event_id"))
    }
    case class send(eventType: String, transactionId: String) {
      val sendEndpoint = s"$roomEndpoint/send/$eventType/$transactionId"
      def put(jsObject: JsObject) = single[String](Put(sendEndpoint, jsObject))(singleStringValueFormat("event_id"))
    }
    object leave {
      val leaveEndpoint = s"$roomEndpoint/leave"
      def post(accessToken: String) = singleToStatus(Post(Uri(leaveEndpoint).withAccessToken(accessToken)))
    }
    object invite {
      val inviteEndpoint = s"$roomEndpoint/invite"
      def post(accessToken: String, userId: String) =
        singleToStatus(Post(Uri(inviteEndpoint).withAccessToken(accessToken), singleStringValueFormat("user_id").write(userId)))
    }
    object ban {
      val banEndpoint = s"$roomEndpoint/ban"
      def post(accessToken: String, userId: String, reason: Option[String] = None) = {
        singleToStatus(Post(banEndpoint, banUserRequest(userId, reason)))
      }
    }
  }

  def profile(userId: UserId) = new ProfileSupport(userId) {
    private val profileEndpoint = s"$versionEndpoint/profile/$userId"
    def displayName = new DisplayNameSupport {
      private val displayNameEndpoint = s"$profileEndpoint/displayName"
      def get() = single[String](Get(displayNameEndpoint))(singleStringValueFormat("displayname"))
      def put(accessToken: String, displayName: String) =
        singleToStatus(Put(displayNameEndpoint, JsObject("displayname" -> displayName.toJson)))
    }
    def avatarUrl = new AvatarUrlSupport {
      val avatarUrlEndpoint = s"$profileEndpoint/avatar_url"
      def get() = single[String](Get(avatarUrlEndpoint))(singleStringValueFormat("avatar_url"))
      def put(accessToken: String, avatarUrl: String) =
        singleToStatus(Put(Uri(avatarUrlEndpoint).withAccessToken(accessToken), JsObject("avatar_url" -> avatarUrl.toJson)))
    }
  }

  /** Closes connection pools without shutting down actor system */
  def close(): Future[Unit] = http.shutdownAllConnectionPools()

  /** Closes connection pools and then shuts down actor system */
  def shutDown(): Future[Terminated] =  close().flatMap(_ => system.terminate())
}

case class ErrorResponseException(errorResponse: ErrorResponse) extends
  Exception(s"${errorResponse.errorCode}: ${errorResponse.error.getOrElse("<no message>")}")

object MatrixJsonProtocol extends DefaultJsonProtocol with ResponseFormats with RequestFormats {

  def loginEntity(user: String, password: String) =
    JsObject("user" -> user.toJson, "password" -> password.toJson, "type" -> "m.login.password".toJson, "medium" -> "email".toJson)

  def registerEntity(userName: String, password: String, bindEmail: Boolean, authenticationData: AuthenticationData) =
    JsObject("username" -> userName.toJson, "password" -> password.toJson, "bind_email" -> bindEmail.toJson,
    "auth" -> authenticationData.toJson)

  implicit lazy val threePidCredsFormat = jsonFormat(ThreePidCredentials, "client_secret", "id_server", "sid")
  def _3pidEntity(threePidCredentials: ThreePidCredentials, bind: Boolean) = JsObject("three_pid_creds" -> threePidCredentials.toJson, "bind" -> bind.toJson)

  def requestTokenEntity(clientSecret: String, idServer: Option[String], sendAttempt: String, email: String) =
    JsObject("client_secret" -> clientSecret.toJson, "id_server" -> idServer.toJson, "send_attempt" -> sendAttempt.toJson, "email" -> email.toJson)
}

abstract class LoginSupport {
  def post(user: String, password: String): Future[LoginResponse]
}
abstract class RegisterEmailSupport {
  def requestToken: RequestTokenSupport
}
abstract class RequestTokenSupport {
  def post(clientSecret: String, idServer: Option[String], sendAttempt: Int, email: String): Future[StatusCode]
}
abstract class ProfileSupport(userId: UserId) {
  def displayName: DisplayNameSupport
  def avatarUrl: AvatarUrlSupport
}
abstract class DisplayNameSupport {
  def get(): Future[String]
  def put(accessToken: String, displayName: String): Future[StatusCode]
}
abstract class AvatarUrlSupport {
  def get(): Future[String]
  def put(accessToken: String, avatarUrl: String): Future[StatusCode]
}
