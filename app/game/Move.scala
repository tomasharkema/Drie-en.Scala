package game

import play.Logger
import play.api.libs.json.{JsObject, JsArray, JsValue}

/**
 * Created by tomas on 08-08-15.
 */

sealed abstract class Move
case class ThrowOnTableMove(cards: Seq[Card]) extends Move
case class ThrowCardFromClosedTableMove(card: Int) extends Move
case class ThrowToOwnClosedMove(card: Card) extends Move
case class GrabFromTableMove() extends Move
case class ThrowCardFromOpenTableMove(cards: Seq[Card]) extends Move

object Move {
  def moveType(move:Move): MoveType= {
    move match {
      case ThrowOnTableMove(_) => ThrowOnTable
      case ThrowCardFromClosedTableMove(_) => ThrowCardFromClosedTable
      case ThrowToOwnClosedMove(_) => ThrowToOwnClosed
      case GrabFromTableMove() => GrabFromTable
      case ThrowCardFromOpenTableMove(_) => ThrowCardFromOpenTable
    }
  }
}

sealed abstract class MoveType(val value: String, val commitMoveOnGame: (Game, Move, Player) => (Either[String, Game], MoveType.ToNextPlayer)) {
  def fromKV(kv: JsValue): Option[Move]
}

object ThrowCardFromOpenTable extends MoveType("ThrowCardFromOpenTable", {(game, move, player) =>
  val correctMove = move.asInstanceOf[ThrowCardFromOpenTableMove]

  if (player.handCards.isEmpty && game.canThrowOnTable(correctMove.cards)) {

    val players = game.players.map { p =>
      if (p.isEqual(player)) {
        val newTableOpenCards = player.openedTableCards.diff(correctMove.cards)

        (player.copy(openedTableCards = newTableOpenCards), !correctMove.cards.map(player.openedTableCards.contains(_)).exists(!_))
      } else {
        (p, true)
      }
    }

    (Right(game.copy(table = Seq(correctMove.cards, game.table).flatten, players = players.map(_._1))), true)
  } else (Left("Kan deze kaart niet opgooien!"), false)
}) {
  override def fromKV(kv: JsValue): Option[ThrowCardFromOpenTableMove] = {
    val asArr = kv.asOpt[Seq[Card]]
    val asObj = kv.asOpt[JsObject].map(_.as[Card])
    (asArr, asObj) match {
      case (Some(arr), _) =>
        Some(ThrowCardFromOpenTableMove(arr))
      case (_, Some(obj)) =>
        Some(ThrowCardFromOpenTableMove(Seq(obj)))
      case _ =>
        None
    }
  }
}

object ThrowOnTable extends MoveType("ThrowOnTable", { (game, move, player) =>
  val correctMove = move.asInstanceOf[ThrowOnTableMove]

  val (newDeck, drawnCards) = game.deck.drawCards(correctMove.cards.size)

  val players = game.players.map { p =>
    if (p.isEqual(player)) {
      val newHandCards = Seq(player.handCards.diff(correctMove.cards),
        if (drawnCards.nonEmpty && (player.handCards.size - correctMove.cards.size) < 3)
          drawnCards
        else
          Seq()
      ).flatten

      (player.copy(handCards = newHandCards), !correctMove.cards.map(player.handCards.contains(_)).exists(!_))
    } else {
      (p, true)
    }
  }

  val newTableCards = Seq(correctMove.cards, game.table).flatten

  if (players.map(_._2).count(b => !b) == 0)
    if (game.canThrowOnTable(correctMove.cards)) {
      (Right(game.copy(deck = newDeck, table = newTableCards, players = players.map(_._1))), !players.map(_._1).find(_.isEqual(player)).get.handCards.exists(_.rank.rank == correctMove.cards.head.rank.rank)) //correctMove.cards.head.rank.rankType != WillThrowAwayAllCards)
    } else
      (Left("Kan deze kaart niet op de tafel gooien!"), true)
  else
    (Left("Niemand is aan zet?"), true)

}) {
  def fromKV(kv: JsValue): Option[ThrowOnTableMove] = {
    val asArr = kv.asOpt[Seq[Card]]
    val asObj = kv.asOpt[JsObject].map(_.as[Card])
    (asArr, asObj) match {
      case (Some(arr), _) =>
        Some(ThrowOnTableMove(arr))
      case (_, Some(obj)) =>
        Some(ThrowOnTableMove(Seq(obj)))
      case _ =>
        None
    }
  }
}

object ThrowCardFromClosedTable extends MoveType("ThrowCardFromClosedTable", { (game, move, player) =>
  val correctMove = move.asInstanceOf[ThrowCardFromClosedTableMove]

  if (player.canThrowClosedCard) {
    val card = player.closedTableCards(correctMove.card)

    val players = game.players.map { p =>
      if (p.isEqual(player)) {
        p.copy(closedTableCards = p.closedTableCards.filter(!_.isEqual(card)))
      } else p
    }

    if (game.canThrowOnTable(Seq(card))) (Right(game.copy(table = Seq(Seq(card), game.table).flatten, players = players)), true)
    else GrabFromTable.commitMoveOnGame(game.copy(table = Seq(Seq(card), game.table).flatten, players = players), GrabFromTableMove(), player)
  } else (Left("Je kan geen gesloten kaart opgooien!"), false)
}) {
  def fromKV(kv: JsValue): Option[ThrowCardFromClosedTableMove] = (kv \ "card").asOpt[Int].map(ThrowCardFromClosedTableMove(_))
}

object ThrowToOwnClosed extends MoveType("ThrowToOwnClosed", {(game, move, player) =>
  val correctMove = move.asInstanceOf[ThrowToOwnClosedMove]
  val players = game.players.map { p =>
    if (p.isEqual(player)) {
      val newOpenTableCards = Seq(player.openedTableCards, Seq(correctMove.card)).flatten
      val newHandCards = player.handCards.filter(_ != correctMove.card)

      (player.copy(openedTableCards = newOpenTableCards, handCards = newHandCards),
        player.handCards.size > 3 &&
          player.openedTableCards.size < 3 &&
          player.handCards.contains(correctMove.card))
    } else (p, true)
  }

  if (players.map(_._2).count(b => !b) == 0)
    (Right(game.copy(players = players.map(_._1))), player.openedTableCards.size > 1)
  else (Left("Niemand is aan de beurt?!"), false)
}) {
  def fromKV(kv: JsValue): Option[ThrowToOwnClosedMove]= Card.fromKV(kv) match {
    case Some(card) => Some(ThrowToOwnClosedMove(card))
    case _ => None
  }
}

object GrabFromTable extends MoveType("GrabFromTable", {(game, _, player) =>
  val players = game.players.map { p =>
    if (p.isEqual(player)) {
      p.copy(handCards = Seq(p.handCards, game.table).flatten)
    } else p
  }

  (Right(game.copy(players = players, table = Seq())), true)
}) {
  def fromKV(kv: JsValue): Option[GrabFromTableMove] = Some(GrabFromTableMove())
}

object MoveType {

  type ToNextPlayer = Boolean

  def fromString(str: String): Option[(JsValue) => Option[Move]] = {
    str match {
      case "ThrowOnTable" => Some((kv: JsValue) => ThrowOnTable.fromKV(kv))
      case "ThrowCardFromClosedTable" => Some((kv: JsValue) => ThrowCardFromClosedTable.fromKV(kv))
      case "ThrowToOwnClosed" => Some((kv: JsValue) => ThrowToOwnClosed.fromKV(kv))
      case "GrabFromTable" => Some((kv: JsValue) => GrabFromTable.fromKV(kv))
      case "ThrowCardFromOpenTable" => Some((kv: JsValue) => ThrowCardFromOpenTable.fromKV(kv))
      case _ => None
    }
  }
}