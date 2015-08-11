package controllers

import game.Game
import play.api._
import play.api.mvc._
import security.Secured
import user.User
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Application extends Controller with Secured {

  def index = withUserFuture { (user, token) => implicit request =>
    Game.getGamesForUser(user).map(games => Ok(views.html.index(user, games)))
  }

  def login = Action {
    Ok(views.html.login())
  }

  def loginPost = Action.async { implicit request =>
    request.body.asFormUrlEncoded match {
      case Some(map) =>

        map.get("name") match {
          case Some(seq) =>

            User.getUserByName(seq.head).map {
              case Some(user) =>
                Redirect(routes.Application.index)
                  .withSession(Security.username -> user.id)
              case _ =>
                Redirect(routes.Application.login())
            }
          case None =>
            Future(Redirect(routes.Application.login()))
        }

      case _ =>
        Future(Redirect(routes.Application.login()))
    }
  }

}