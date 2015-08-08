package controllers

import game._
import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.cache._
import play.api.Play.current
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

object GameController extends Controller {

  private def getByCacheOrFetch(id: String): Future[Option[Game]] = {
    Cache.getAs[Game]("gameuid:" + id) match {
      case Some(game: Game) =>
        Future.apply(Some(game))
      case None =>
        Game.getByID(id).map {
          case Some(game: Game) =>
            Cache.set("gameuid:" + id, game)
            Some(game)
          case None =>
            None
        }
    }
  }
  // http://localhost:5555/game/id/c87c87dc-f5de-46bc-b91d-e679bd99a63a
  def game(gameId: String) = Action.async {
    getByCacheOrFetch(gameId).map {
      case Some(game) =>
        Ok(views.html.game(game))
      case None =>
        NotFound
    }
  }

  def newGame(redirect: Option[Boolean]) = Action {
    val id = java.util.UUID.randomUUID.toString
    val game = Game.newGame(id)
    Cache.set("gameuid:" + id, game)

    redirect match {
      case Some(true) => Redirect(routes.GameController.game(game.id))
      case _ => Ok(Json.toJson(game))
    }
  }

  def gameState(gameId: String) = Action.async {
    getByCacheOrFetch(gameId).map {
      case Some(game) =>
        Ok(Json.toJson(game))
      case None =>
        NotFound
    }
  }

  // http://localhost:5555/game/commitMove?gameId=c87c87dc-f5de-46bc-b91d-e679bd99a63a&playerId=A&move=ThrowToOwnClosed&kv=suit:R,rank:Q
  // http://localhost:5555/game/commitMove?redirect=true&gameId=9b8aeb0e-9c0a-4bbf-a184-a4ecd71cb9c9&playerId=D&move=ThrowOnTable&kv=%5B%7B%22suit%22%3A%22R%22%2C%22rank%22%3A%229%22%7D%2C%7B%22suit%22%3A%22H%22%2C%22rank%22%3A%224%22%7D%5D

  private def toKV(kvString: String): Map[String, String] = kvString.split(",").toSeq.map(_.split(":").toSeq).map(s => s.head -> s(1)).toMap

  def commitMove(redirect: Option[Boolean], gameId: String, playerId: String, moveString: String, kvString: String) = Action.async {
    Logger.info(playerId + " doet " + moveString)

    Game.getByID(gameId).map {
      case Some(game: Game) =>

        val res = for {
          player <- game.players.find(_.id == playerId)
          move <- MoveType.fromString(moveString).flatMap(_(Json.parse(kvString)))
        } yield game.doTurn(move, player)

        res match {
          case Some(Some(game)) =>

            Game.saveState(game)
            Cache.set("gameuid:" + game.id, game)

            redirect match {
              case Some(true) => Redirect(routes.GameController.game(game.id))
              case _ => Ok(Json.toJson(game))
            }

          case Some(None) =>
            NotFound("No turn found")
          case None =>
            NotFound("Player or move not found")
        }

      case None =>
        NotFound("Game niet gevonden")
    }
  }
}
