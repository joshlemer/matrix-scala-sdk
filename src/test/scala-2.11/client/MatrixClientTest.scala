package client

import org.scalatest.{AsyncFlatSpec, Matchers}
import request.{AuthenticationData, UserKind}
import response.RegisterResponse

import scala.util.Success

class MatrixClientTest extends AsyncFlatSpec with Matchers {

  val client = new MatrixClient("http://localhost:8008")
  implicit val ec = client.ec


  behavior of "MatrixClient"

  it should "Get a list of versions supported by the Matrix Server" in {
    client.versions.get().map(seq => seq should contain allElementsOf List("r0.0.1", "r0.1.0", "r0.2.0"))
  }

  it should "Register a guest" in {
    client.r0.register.post(
      UserKind.Guest, "someUserName", "someUserPassword", bindEmail = false,
      AuthenticationData(None, "example.type.foo")).map{res =>
      res should matchPattern { case RegisterResponse(_, "localhost", _, None) => } }
  }

  it should "Register a new user" in {
    client.r0.register.post(
      UserKind.User, "someUserName", "someUserPassword", bindEmail = false)
      .andThen {
        case Success(RegisterResponse(accessToken, _,userId,_)) =>
          //client.r0.account.deactivate.post(accessToken, )
        case x => println(x)
      }
      .map{ res =>
      println(res)
      res should matchPattern { case RegisterResponse(_, "localhost", _, None) => }
    }
  }

}
