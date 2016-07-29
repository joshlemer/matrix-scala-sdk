import enumeratum._

case class LoginResponse(accessToken: String, homeServer: String, userId: String, refreshToken: String)
case class RegisterResponse(accessToken: String, homeServer: String, userId: String, refreshToken: String)
case class TokenRefreshResponse(accessToken: String, refreshToken: Option[String])

case class ThirdPartyIdentifier(medium: String, address: String)
case class _3pidResponse(threePids: List[ThirdPartyIdentifier])

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
