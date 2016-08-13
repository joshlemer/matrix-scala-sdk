package response

import enumeratum._
import spray.json.DefaultJsonProtocol

case class LoginResponse(accessToken: String, homeServer: String, userId: String, refreshToken: String)
case class RegisterResponse(accessToken: String, homeServer: String, userId: String, refreshToken: String)
case class TokenRefreshResponse(accessToken: String, refreshToken: Option[String])

case class ThirdPartyIdentifier(medium: String, address: String)
case class _3pidResponse(threePids: List[ThirdPartyIdentifier])
case class FilterResponse(filterId: String)

/////////////////////////////////////////////////////////////////////////////
///       SyncResponse objects
/////////////////////////////////////////////////////////////////////////////

case class SyncResponse(nextBatch: String, rooms: Rooms, presence: Presence)
case class Rooms(leave: Map[RoomId, LeftRoom], join: Map[RoomId, JoinedRoom], invite: Map[RoomId, InvitedRoom])
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
  originServerTs: Option[Int],
  sender: Option[String],
  _type: String,
  unsigned: Option[Unsigned],
  stateKey: Option[String]
  )
case class Unsigned(prevContent: Option[EventContent], age: Option[Int], transactionId: Option[String])
case class EventContent(thirdPartyInvite: Option[Invite], membership: Option[Membership], avatarUrl: Option[String], displayName: Option[String])

sealed trait Membership extends EnumEntry
object Membership extends Enum[Membership]{
  case object invite extends Membership
  case object join extends Membership
  case object knock extends Membership
  case object leave extends Membership
  case object ban extends Membership
  val values = findValues
}

case class Invite(displayName: String, signed: Signed)
case class Signed(token: String, signatures: Map[String, Map[String, String]], mxid: String)

sealed trait EventFormat extends EnumEntry
object EventFormat extends Enum[EventFormat] {
  case object Client extends EventFormat
  case object Federation extends EventFormat
  val values = findValues
}

/////////////////////////////////////////////////////////////////////////////
///       ErrorCodes objects
/////////////////////////////////////////////////////////////////////////////

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
  case object M_UNRECOGNIZED extends ErrorCode
  case class Other(override val entryName: String) extends ErrorCode
  val values = findValues
}

case class ErrorResponse(errorCode: ErrorCode, error: Option[String])

trait ResponseFormats extends DefaultJsonProtocol {
  import spray.json._

  lazy val roomIdFormat = new RootJsonFormat[String] {
    def read(json: JsValue) = fromField[String](json, "room_id")
    def write(roomId: String) = JsObject("room_id" -> roomId.toJson)
  }

  implicit lazy val membershipFormat = new JsonFormat[Membership] {
    def read(json: JsValue) = json match {
      case JsString(value) => Membership.withName(value)
      case _ => deserializationError("memberships must be Strings")
    }
    def write(membership: Membership) = JsString(membership.entryName)
  }

  implicit lazy val syncResponseFormat = {
    implicit lazy val signedFormat = jsonFormat3(Signed)
    implicit lazy val inviteFormat = jsonFormat(Invite, "display_name", "signed")
    implicit lazy val eventContentFormat = jsonFormat(EventContent, "third_party_invite", "membership", "avatar_url", "display_name")
    implicit lazy val unsignedFormat = jsonFormat(Unsigned, "prev_content", "age", "transaction_id")
    implicit lazy val eventFormat = jsonFormat(Event, "content", "origin_server_ts", "sender", "type", "unsigned", "state_key")
    implicit lazy val inviteStateFormat = jsonFormat1(InviteState)
    implicit lazy val ephemeralFormat = jsonFormat1(Ephemeral)
    implicit lazy val accountDataFormat = jsonFormat1(AccountData)
    implicit lazy val stateFormat = jsonFormat1(State)
    implicit lazy val timelineFormat = jsonFormat(Timeline, "limited", "prev_batch", "events")
    implicit lazy val unreadNotificationsCountsFormat = jsonFormat(UnreadNotificationCounts, "highlight_count", "notification_count")
    implicit lazy val invitedRoomFormat = jsonFormat(InvitedRoom, "invite_state")
    implicit lazy val leftRoomFormat = jsonFormat2(LeftRoom)
    implicit lazy val joinedRoomFormat =  jsonFormat(JoinedRoom, "unread_notifications", "timeline", "state", "account_data", "ephemeral")
    implicit lazy val roomsFormat = jsonFormat(Rooms, "leave", "join", "invite")
    implicit lazy val presenceFormat = jsonFormat1(Presence)
    jsonFormat(SyncResponse, "next_batch", "rooms", "presence")
  }
}

