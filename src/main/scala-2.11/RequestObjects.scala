package request
import enumeratum._
import response.EventFormat

case class AuthenticationData(session: String, _type: String)

case class ThreePidCredentials(clientSecret: String, idServer: String, sid: String)

case class RoomEventFilter(
  notTypes: List[String],
  notRooms: List[String],
  limit: Int,
  rooms: List[String],
  notSenders: List[String],
  senders: List[String],
  types: List[String])

case class RoomFilter(
  rooms: List[String],
  includeLeave: Boolean,
  accountData: RoomEventFilter,
  timeline: RoomEventFilter,
  ephemeral: RoomEventFilter,
  state: RoomEventFilter,
  notRooms: List[String] = Nil)

case class Filter( notTypes: List[String], limit: Int, senders: List[String], types: List[String], notSenders: List[String])

case class FilterPostRequest( eventFields: List[String], eventFormat: EventFormat, accountData: Filter, room: RoomFilter, presence: Filter)

sealed trait UserKind extends EnumEntry

object UserKind extends Enum[UserKind] {
  case object User extends UserKind
  case object Guest extends UserKind

  val values = findValues
}

sealed trait Presence extends EnumEntry

case object Presence extends Enum[Presence] {
  case object Offline extends Presence
  val values = findValues
}
