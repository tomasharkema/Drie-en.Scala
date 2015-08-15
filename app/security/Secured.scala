package security

import play._
import play.api._
import play.api.mvc._
import play.core.Router
import user.User

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by tomas on 10-08-15.
 */

trait Secured {
  self: Controller =>

  def token(request: RequestHeader): Option[String] = request.session.get(Security.username) match {
    case None =>
      request.getQueryString("userToken")
    case Some(s) => Some(s)
  }

  def onUnauthorized(request: RequestHeader) = Results.Redirect(controllers.routes.Application.login)
  def onUnauthorizedFuture(request: RequestHeader) = Future.apply(onUnauthorized(request))

  def withAuthFuture(f: => String => Request[AnyContent] => Future[Result]) = {
    Security.Authenticated(token, onUnauthorized) { user =>
      Action.async(request => f(user)(request))
    }
  }

  def withUserFuture(f: (User, String) => Request[AnyContent] => Future[Result]) = withAuthFuture { token => implicit request =>
    User.getUser(token).flatMap {
      case Some(user) =>
        f(user, token)(request)
      case None =>
        onUnauthorizedFuture(request)
    }
  }
}
