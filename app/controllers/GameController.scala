package controllers

import controllers.Application._
import game._
import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.cache._
import play.api.Play.current
import reactivemongo.bson.BSONDocument
import user.User
import scala.concurrent.Future
import security.Secured

import scala.concurrent.ExecutionContext.Implicits.global

object GameController extends Controller with Secured {

  def fetchGameAndUser(game: Game): Future[Seq[User]] =
    User.getUserByIds(game.players.map(_.id))

  private def getByCacheOrFetch(id: String): Future[Option[(Game, Seq[User])]] = {
    Cache.getAs[Game]("gameuid:" + id) match {
      case Some(game: Game) =>

        fetchGameAndUser(game).map(users => Some((game, users)))
      case None =>
        Game.getByID(id).flatMap {
          case Some(game: Game) =>
            fetchGameAndUser(game).map(users => Some((game, users)))
          case _ =>
            Future(None)
        }
    }
  }

  def game(gameId: String) = withUserFuture { (currentUser, token) => implicit request =>
    getByCacheOrFetch(gameId).map {
      case Some((game, users)) =>
        Ok(views.html.game(game, users, currentUser))
      case None =>
        NotFound
    }
  }

  def newGame(redirect: Option[Boolean] = None) = Action {
    val id = java.util.UUID.randomUUID.toString
    val game = Game.newGame(id)
    Cache.set("gameuid:" + id, game)

    redirect match {
      case Some(true) => Redirect(routes.GameController.game(game.id))
      case _ => Ok(Json.toJson(game))
    }
  }

  def addNewGame = withUserFuture { (currentUser, token) => implicit request =>
    Future(Ok(views.html.addGame()))
  }

  def addGamePost = withUserFuture { (currentUser, token) => implicit request =>

    request.body.asFormUrlEncoded match {
      case Some(map) =>

        Future.sequence(Seq(map.get("user1").map(_(0)), map.get("user2").map(_(0)), map.get("user3").map(_(0)))
          .filter(_.isDefined).map(_.get).filter(!_.equals(""))
          .map { userName =>
            User.getUserByName(userName)
          }).map { users =>

            if (users.exists(userExists => userExists.isEmpty || userExists.exists(_.id.equals(currentUser.id)))) {
              // een user is niet gevonden, of hetzelfde als de user die hem aanmaakt.
              Redirect(routes.GameController.addNewGame())
            } else {
              val game = Game.newGameWithUsers(Seq(users.map(_.get), Seq(currentUser)).flatten)
              Redirect(routes.GameController.game(game.id))
            }

          }
      case _ =>
        Future(Redirect(routes.GameController.addNewGame()))
    }
  }

  def gameState(gameId: String) = withUserFuture { (currentUser, token) => implicit request =>
    getByCacheOrFetch(gameId).map {
      case Some((game, users)) =>
        Ok(Json.toJson(game))
      case None =>
        NotFound
    }
  }

  private def toKV(kvString: String): Map[String, String] = kvString.split(",").toSeq.map(_.split(":").toSeq).map(s => s.head -> s(1)).toMap

  def commitMove(redirect: Option[Boolean], gameId: String, moveString: String, kvString: String) = withUserFuture { (currentUser, token) => implicit request =>
    Logger.info(currentUser + " doet " + moveString)

    Game.getByID(gameId).map {
      case Some(game: Game) =>

        val res = for {
          player <- game.players.find(_.id == currentUser.id)
          move <- MoveType.fromString(moveString).flatMap(_(Json.parse(kvString)))
        } yield game.doTurn(move, player)

        res match {
          case Some(Right(game)) =>

            Game.saveState(game)
            Cache.set("gameuid:" + game.id, game)

            redirect match {
              case Some(true) => Redirect(routes.GameController.game(game.id))
              case _ => Ok(Json.toJson(game))
            }

          case Some(Left(message)) =>
            NotFound(message)
          case None =>
            NotFound("Player or move not found")
        }

      case None =>
        NotFound("Game niet gevonden")
    }
  }
}
