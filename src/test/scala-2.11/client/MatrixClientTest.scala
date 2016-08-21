package client

import akka.http.scaladsl.model.StatusCodes
import org.scalatest.{AsyncFlatSpec, Matchers}
import request.{AuthenticationData, UserKind}
import response.{TokenRefreshResponse, LoginResponse, RegisterResponse}

class MatrixClientTest extends AsyncFlatSpec with Matchers {

  val client = new MatrixClient("http://localhost:8008")
  implicit val ec = client.ec

  behavior of "MatrixClient"

  it should "Get a list of versions supported by the Matrix Server" in {
    client.versions.get().map(seq => seq should contain allElementsOf List("r0.0.1", "r0.1.0", "r0.2.0"))
  }

  it should "Register a guest" in {
    client.register.post(
      UserKind.Guest, "someUserName", "someUserPassword", bindEmail = false,
      AuthenticationData(None, "example.type.foo")
    ).map{res =>
      res should matchPattern { case RegisterResponse(_, "localhost", _, None) => }
    }
  }

  it should "Register a new user" in {
    client.register.post(
      UserKind.User, "someUserName", "someUserPassword", bindEmail = false).map{ res =>
      res should matchPattern { case RegisterResponse(_, "localhost", _, Some(_)) => }
    }
  }

  it should "Login an existing user" in {
    client.login.post("someUserName", "someUserPassword").map { res =>
      res should matchPattern { case LoginResponse(_, "localhost", _, _) => }
    }
  }

  it should "Fail to login a fake user" in {
    client.login.post("doesNotExist", "123454").failed.map { f =>
      f should matchPattern { case ErrorResponseException(_) => }
    }
  }

  it should "Log out a logged-in user" in {
    client.login.post("someUserName", "someUserPassword")
    .flatMap( loginRes =>
      client.logout.post(loginRes.accessToken)
    ).map(statusCode => statusCode.isSuccess should be(true))
  }

//  it should "Receive a success when requesting a token" in {
//    client.register.email.requestToken.post("somesuperdupersecretsecret",None,1,"joshlemer@gmail.com").map{ res =>
//      res should be (StatusCodes.Success)
//    }
//  }

  it should "Receive successfully refresh a token for an existing user" in {
    client.login.post("someUserName", "someUserPassword")
    .flatMap{loginRes =>
      println(loginRes)
      client.tokenRefresh.post(loginRes.accessToken, loginRes.refreshToken)
    }.map{refreshRes =>
      println(refreshRes)
      refreshRes should matchPattern { case TokenRefreshResponse(_, _) => }
    }
  }


}
