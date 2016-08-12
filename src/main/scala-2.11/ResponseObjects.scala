package response
import enumeratum._
import spray.json.{DefaultJsonProtocol, JsObject}

case class LoginResponse(accessToken: String, homeServer: String, userId: String, refreshToken: String)
case class RegisterResponse(accessToken: String, homeServer: String, userId: String, refreshToken: String)
case class TokenRefreshResponse(accessToken: String, refreshToken: Option[String])

case class ThirdPartyIdentifier(medium: String, address: String)
case class _3pidResponse(threePids: List[ThirdPartyIdentifier])
case class FilterResponse(filterId: String)
case class SyncResponse(nextBatch: String, rooms: Rooms, presence: Presence)
case class Rooms(leftRoomId: String, leftRoom: LeftRoom)
case class LeftRoom(timeline: Timeline, state: State)
case class JoinedRoom(
  unreadNotifications: UnreadNotificationCounts,
  timeline: Timeline,
  state: State,
  accountData: AccountData,
  ephemeral: Ephemeral
  )

case class UnreadNotificationCounts(highlightCount: Int, notificationCount: Int)
case class Timeline(limited: Boolean, prevBatch: String, events: List[Event])
case class State(events: List[Event])
case class AccountData(events: List[Event])
case class Ephemeral(events: List[Event])
case class InvitedRoom(inviteState: InviteState)
case class InviteState(events: List[Event])
case class Presence(events: List[Event])
case class Event(
  content: EventContent,
  originServerTs: Int,
  sender: String,
  _type: String,
  unsigned: Unsigned,
  stateKey: String
  )
case class Unsigned(prevContent: EventContent, age: Int, transactionId: String)
case class EventContent(fields: JsObject)

sealed trait EventFormat extends EnumEntry
object EventFormat extends Enum[EventFormat] {
  case object Client extends EventFormat
  case object Federation extends EventFormat
  val values = findValues
}

sealed trait ErrorCode extends EnumEntry

object ErrorCode extends Enum[ErrorCode] {

  case object M_FORBIDDEN extends ErrorCode
  case object M_UNKNOWN_TOKEN extends ErrorCode
  case object M_BAD_JSON extends ErrorCode
  case object M_NOT_JSON extends ErrorCode
  case object M_NOT_FOUND extends ErrorCode
  case object M_LIMIT_EXCEEDED extends ErrorCode
  case object M_USER_IN_USE extends ErrorCode
  case object M_INVALID_USERNAME extends ErrorCode
  case object M_ROOM_IN_USE extends ErrorCode
  case object M_THREEPID_IN_USE extends ErrorCode
  case object M_THREEPID_NOT_FOUND extends ErrorCode
  case object M_SERVER_NOT_TRUSTED extends ErrorCode

  val values = findValues
}

case class ErrorResponse(errorCode: ErrorCode, error: Option[String])

trait ResponseFormats extends DefaultJsonProtocol {
  import spray.json._

  lazy val roomIdFormat: RootJsonFormat[String] = new RootJsonFormat[String] {
    def read(json: JsValue) = fromField[String](json, "room_id")
    def write(roomId: String) = JsObject("room_id" -> roomId.toJson)
  }

}

