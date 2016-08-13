package request
import enumeratum._
import response.EventFormat
import spray.json._

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

sealed abstract class Presence(override val entryName: String) extends EnumEntry

object Presence extends Enum[Presence] {
  case object Offline extends Presence("offline")
  val values = findValues
}

sealed abstract class Visibility(override val entryName: String) extends EnumEntry
object Visibility extends Enum[Visibility] {
  case object Public extends Visibility("public")
  case object Private extends Visibility("private")
  val values = findValues
}

sealed abstract class Preset(override val entryName: String) extends EnumEntry
object Preset extends Enum[Preset] {
  /** join_rules is set to `invite`
    * history_visibility is set to `shared` */
  case object PrivateChat extends Preset("private_chat")
  /** join_rules is set to `invite`
    * history_visibility is set to `shared`
    * all invitees are given the same power level as the room creator*/
  case object TrustedPrivateChat extends Preset("trusted_private_chat")
  /** join_rules is set to `public`
    * history_visibility is set to `shared` */
  case object PublicChat extends Preset("trusted_private_chat")

  val values = findValues
}

case class Invite3pid(idServer: String, medium: String, address: String)

case class StateEvent(content: String, _type: String, stateKey: String)

trait RequestFormats extends DefaultJsonProtocol {

  def enumFormat[T <: EnumEntry](enum: Enum[T]): JsonFormat[T] = new JsonFormat[T] {
    def read(json: JsValue) = json match { case JsString(value) => enum.withName(value) case _ => deserializationError("enum must be string")}
    def write(entry: T) = JsString(entry.entryName)
  }

  implicit lazy val invite3pidFormat = jsonFormat(Invite3pid, "id_server", "medium", "address")
  implicit lazy val stateEventFormat = jsonFormat(StateEvent, "content", "type", "state_key")


  def createRoomRequest(
    preset: Preset, invite: Seq[String], name: Option[String], visibility: Visibility,
    invite3pids: Seq[Invite3pid], topic: Option[String], initialState: Seq[StateEvent],
    roomAliasName: Option[String]
    ) = {
    val fields = Map("preset" -> preset.toJson(enumFormat(Preset)), "visibility" -> visibility.toJson(enumFormat(Visibility)), "invite_3pid" -> invite3pids.toJson,
      "initial_state" -> initialState.toJson
    ) ++ name.map("name" -> _.toJson) ++ topic.map("topic" -> _.toJson) ++ roomAliasName.map("room_alias_name" -> _.toJson)

    JsObject(fields)
  }

  implicit lazy val authenticationDataFormat = jsonFormat(AuthenticationData, "session", "type")
}