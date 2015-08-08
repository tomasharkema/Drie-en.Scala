package game

import org.joda.time.DateTime
import play.api.cache.Cache
import reactivemongo.bson._

import scala.concurrent.Future
import util.Random.shuffle
import play.api.libs.json._
import connection.Connection

import scala.concurrent.ExecutionContext.Implicits.global

case class Player(id: String,
                  turn: Boolean,
                  closedTableCards: Seq[Card],
                  openedTableCards: Seq[Card],
                  handCards: Seq[Card])

object Player {

  def newPlayer(id: String, deck: Deck): Player = {
    Player(id,
      false,
      Seq(),//Seq(deck.draw().get, deck.draw().get, deck.draw().get),
      Seq(),
      Seq())//Seq(deck.draw().get, deck.draw().get, deck.draw().get, deck.draw().get, deck.draw().get, deck.draw().get))
  }

  implicit val writesPlayer: Writes[Player] = new Writes[Player] {
    override def writes(player: Player): JsValue = {
      Json.obj(
        "id" -> player.id,
        "turn" -> player.turn,
        "closedTableCards" -> Json.toJson(player.closedTableCards),
        "openedTableCards" -> Json.toJson(player.openedTableCards),
        "handCards" -> Json.toJson(player.handCards)
      )
    }
  }

  implicit val writesPlayerBSON = new BSONDocumentWriter[Player] {
    def write(player: Player) = BSONDocument(
      "id" -> player.id,
      "turn" -> player.turn,
      "closedTableCards" -> player.closedTableCards,
      "openedTableCards" -> player.openedTableCards,
      "handCards" -> player.handCards
    )
  }

  implicit val readsPlayerBSON = new BSONDocumentReader[Player] {
    override def read(bson: BSONDocument): Player = Player(
      bson.getAs[String]("id").get,
      bson.getAs[Boolean]("turn").get,
      bson.getAs[Seq[Card]]("closedTableCards").get,
      bson.getAs[Seq[Card]]("openedTableCards").get,
      bson.getAs[Seq[Card]]("handCards").get
    )
  }
}

case class Deck(cards: Seq[Card]) {

  def drawCards(count: Int): (Deck, Seq[Card]) = {
    val drawnCards = cards.splitAt(cards.size - count)._2
    val newDeck = copy(cards = cards.diff(drawnCards))

    (newDeck, drawnCards)
  }

  def draw(): (Deck, Option[Card]) = {
    val card = cards.lastOption

    val newDeck = if (card.isDefined) copy(cards = cards.diff(Seq(card.get))) else this

    (newDeck, card)
  }
}

object Deck {
  def newDeck = {
    val cards = Suit.Suits.flatMap { s =>
      Rank.Ranks.map { r =>
        Card(s, r)
      }
    }

    Deck(shuffle(cards))
  }

  implicit val writesDeck: Writes[Deck] = new Writes[Deck] {
    override def writes(deck: Deck): JsValue = {
      Json.obj(
        "cards" -> Json.toJson(deck.cards)
      )
    }
  }

  implicit val writesDeckBSON = new BSONDocumentWriter[Deck] {
    def write(deck: Deck) = BSONDocument(
      "cards" -> deck.cards
    )
  }

  implicit val readsDeckBSON = new BSONDocumentReader[Deck] {
    override def read(bson: BSONDocument): Deck = Deck(
      bson.getAs[Seq[Card]]("cards").get
    )
  }
}

sealed abstract class GameState(val name: String, val isInState: Game => Boolean, val moves: Set[MoveType], val precedes: Set[GameState])
case object Starting extends GameState("Starting", { game =>
  game.players.map(_.openedTableCards.size).exists(_ < 3) &&
    game.table.isEmpty &&
    !game.players.map(_.handCards.size).exists(_ > 6) &&
    game.truncated.size < 1
}, Set(ThrowToOwnClosed), Set())
case object FirstMover extends GameState("FirstMover", { game =>
  game.players.map(_.openedTableCards.size).count(_ == 3) == game.players.size &&
  game.table.isEmpty &&
  game.players.map(_.handCards).count(_ == 3) == game.players.size
}, Set(ThrowOnTable), Set(Starting))
case object NormalThrowing extends GameState("NormalThrowing", { game =>
  //!game.players.map(_.handCards.size).contains(0) &&
    !game.players.map(_.openedTableCards.size).contains(0) &&
    !game.players.map(_.closedTableCards.size).contains(0)
}, Set(ThrowOnTable, GrabFromTable, ThrowCardFromOpenTable), Set(FirstMover))

object GameState {
  val States = Seq(Starting, FirstMover, NormalThrowing)

  def fromString(string: String): Option[GameState] = {
    States.find(_.name == string)
  }

  implicit val writesGameStateBSON = new BSONDocumentWriter[GameState] {
    def write(gameState: GameState) = BSONDocument("state" -> gameState.name)
  }

  implicit val readsGameStateBSON = new BSONDocumentReader[GameState] {
    override def read(bson: BSONDocument): GameState = GameState.fromString(bson.getAs[String]("state").get).get
  }
}

case class Game(id: String,
                deck: Deck,
                players: Seq[Player],
                table: Seq[Card],
                truncated: Seq[Card],
                lastUpdated: DateTime,
                passedState: Set[GameState]) {

  def draw: Game = {

    val (newDeck, cards) = deck.drawCards(players.size * 9)

    val newPlayers = players.map { player =>
      val cardsForPlayer = cards.slice((players.indexOf(player) + 0) * 9, (players.indexOf(player) + 1) * 9)
      val (handCards, closedTableCards) = cardsForPlayer.splitAt(6)
      player.copy(handCards = handCards, closedTableCards = closedTableCards)
    }

    copy(players = newPlayers, deck = newDeck)
  }

  def isInState: GameState = {
    val validStates = GameState.States
      .filter(_.isInState(this))
      .filter(!passedState.contains(_))

    validStates.head
  }

  def validMovesForState = {
    isInState.moves
  }

  def isCorrectMoveForState(move: MoveType): Boolean = {
    validMovesForState.exists { moveType =>
      moveType.isInstanceOf[move.type]
    }
  }

  def aboveSignificantCardOnTable = table.find(_.rank.rankType != Joker)

  def canThrowOnTable(cards: Seq[Card]): Boolean = {
    val allTheSame = cards.map(_.rank).count(_ == cards.head.rank) == cards.size
    val isNextForHeadTableCard = aboveSignificantCardOnTable.map(_.comesAfter(cards.head))

    if (allTheSame) {
      isNextForHeadTableCard match {
        case Some(bool) => bool
        case None => true
      }
    } else {
      false
    }
  }

  def nextPlayer: Game = {
    val playerOnTurn = players.find(_.turn).get
    val playerOnTurnIndex = players.indexOf(playerOnTurn)
    val newPlayerOnTurnIndex = if ((playerOnTurnIndex + 1) == players.size) 0 else playerOnTurnIndex + 1

    val newPlayers = players.map { player =>
      val index = players.indexOf(player)

      if (index == playerOnTurnIndex) {
        player.copy(turn = false)
      } else if (index == newPlayerOnTurnIndex) {
        player.copy(turn = true)
      } else {
        player
      }
    }

    this.copy(players = newPlayers)
  }

  def doTurn(move: Move, player: Player): Option[Game] = {
    val moveType = Move.moveType(move)

    if (isCorrectMoveForState(moveType) && player.turn) {
      val committedMove = moveType.commitMoveOnGame(this, move, player) match {
        case (Some(game), true) =>
          Some(game.nextPlayer)
        case (Some(game), false) =>
          Some(game)
        case (None, _) =>
          None
      }

      committedMove match {
        case Some(game) =>

          // Commit after game commit things

          val firstCard = game.table.headOption
          val (newTruncatedCards, newTableCards) = firstCard match {
            case Some(card) =>
              if (card.rank.rankType == WillThrowAwayAllCards)
                (Seq(game.truncated, game.table).flatten, Seq())
              else
                (game.truncated, game.table)
            case _ =>
              (game.truncated, game.table)
          }

          val newPassedState = Set(game.passedState, game.isInState.precedes).flatten

          Some(game.copy(truncated = newTruncatedCards, table = newTableCards, passedState = newPassedState))
        case _ => None
      }
    } else {
      None
    }
  }
}

object Game {

  val gameCollection = Connection.getCollection("games")

  def newGame(id: String): Game = {
    val deck = Deck.newDeck
    val game = Game(
      id = id,
      deck = deck,
      players = Seq(Player.newPlayer("Vlad", deck).copy(turn = true), Player.newPlayer("Freddie", deck), Player.newPlayer("Tomas", deck)),
      table = Seq(),
      truncated = Seq(),
      lastUpdated = DateTime.now(), Set()).draw
    gameCollection.insert(BSON.write(game))
    game
  }

  def saveState(game: Game) = {
    gameCollection.update(BSONDocument("id" -> game.id), BSON.write(game))
  }

  def getByID(id: String): Future[Option[Game]] = {
    gameCollection
      .find(BSONDocument("id" -> id))
      .cursor[Game].collect[List]()
      .map(_.headOption)
  }

  implicit val writesGamesJson: Writes[Game] = new Writes[Game] {
    override def writes(game: Game): JsValue = Json.obj(
        "id" -> game.id,
        "deck" -> Json.toJson(game.deck),
        "players" -> Json.toJson(game.players),
        "table" -> Json.toJson(game.table),
        "truncated" -> Json.toJson(game.truncated),
        "lastUpdated" -> game.lastUpdated.getMillis,
        "gameState" -> game.isInState.name
      )
  }

  implicit val writesGamesBSON = new BSONDocumentWriter[Game] {
    def write(game: Game) = BSONDocument(
      "id" -> game.id,
      "deck" -> BSON.write(game.deck),
      "players" -> game.players,
      "table" -> game.table,
      "truncated" -> game.truncated,
      "lastUpdated" -> game.lastUpdated.getMillis,
      "passedState" -> game.passedState
    )
  }

  implicit val readsGameBSON = new BSONDocumentReader[Game] {
    override def read(bson: BSONDocument): Game = Game(
      bson.getAs[String]("id").get,
      bson.getAs[Deck]("deck").get,
      bson.getAs[Seq[Player]]("players").get,
      bson.getAs[Seq[Card]]("table").get,
      bson.getAs[Seq[Card]]("truncated").get,
      new DateTime(bson.getAs[Long]("lastUpdated").get),
      bson.getAs[Set[GameState]]("passedState").get
    )
  }
}