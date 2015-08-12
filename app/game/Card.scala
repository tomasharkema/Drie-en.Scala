package game

import play.api.libs.json._
import reactivemongo.bson.{BSONDocumentReader, BSONDocument, BSONDocumentWriter}
import play.api.libs.functional.syntax._

/**
 * Created by tomas on 08-08-15.
 */

sealed abstract class Suit(val icon: String, val string: String)
case object Hearts extends Suit("♥", "H")
case object Diamonds extends Suit("♦", "R")
case object Clubs extends Suit("♣", "L")
case object Spades extends Suit("♠", "S")

object Suit {
  val Suits = Seq(Hearts, Diamonds, Clubs, Spades)
  def fromString(suit: String) = Suits.find(_.string.equalsIgnoreCase(suit))

  implicit val suitReads = new Reads[Suit] {
    override def reads(json: JsValue): JsResult[Suit] = {
      fromString(json.as[String]) match {
        case Some(suitUnwrap) => JsSuccess(suitUnwrap)
        case None => JsError("Suit not valid")
      }
    }
  }
}

sealed abstract class RankType(val string: String)
case object Normal extends RankType("Normal")
case object Joker extends RankType("Joker")
case object LowerThan extends RankType("LowerThan")
case object WillThrowAwayAllCards extends RankType("WillThrowAwayAllCards")
case object StartAgain extends RankType("StartAgain")

sealed abstract class Rank(val rank: Int, val abbr: String, val rankType: RankType)
case object Two extends Rank(2, "2", StartAgain)
case object Three extends Rank(3, "3", Joker)
case object Four extends Rank(4, "4", Normal)
case object Five extends Rank(5, "5", Normal)
case object Six extends Rank(6, "6", Normal)
case object Seven extends Rank(7, "7", LowerThan)
case object Eight extends Rank(8, "8", Normal)
case object Nine extends Rank(9, "9", Normal)
case object Ten extends Rank(10, "10", WillThrowAwayAllCards)
case object Jack extends Rank(11, "J", Normal)
case object Queen extends Rank(12, "Q", Normal)
case object King extends Rank(13, "K", Normal)
case object Ace extends Rank(14, "A", Normal)

object Rank {
  val Ranks = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

  def fromString(rank: String) = Ranks.find(_.abbr.equalsIgnoreCase(rank))

  implicit val rankReads = new Reads[Rank] {
    override def reads(json: JsValue): JsResult[Rank] = {
      fromString(json.as[String]) match {
        case Some(suitUnwrap) => JsSuccess(suitUnwrap)
        case None => JsError("Rank not valid")
      }
    }
  }
}

case class Card(suit: Suit,
                rank: Rank) {
  def string = this.suit.icon + this.rank.abbr

  def isEqual(otherCard: Card): Boolean = {
    this.rank.rank == otherCard.rank.rank && this.suit.string.equals(otherCard.suit.string)
  }

  def comesAfter(otherCard: Card): Boolean = {
    rank.rankType match {
      case Normal =>
        otherCard.rank.rank >= this.rank.rank
      case StartAgain =>
        otherCard.rank.rank >= this.rank.rank
      case LowerThan =>
        otherCard.rank.rank <= this.rank.rank
      case _ =>
        true
    }
  }
}

object Card {
  def fromKV(kv: JsValue): Option[Card] = {
    kv.asOpt[JsObject] match {
      case Some(obj) =>
        (Suit.fromString((obj \ "suit").as[String]), Rank.fromString((obj \ "rank").as[String])) match {
          case (Some(suit), Some(rank)) => Some(Card(suit, rank))
          case _ => None
        }
      case _ => None
    }
  }

  implicit val writesCard: Writes[Card] = new Writes[Card] {
    override def writes(card: Card): JsValue = {
      Json.obj(
        "string" -> card.string,
        "suit" -> card.suit.string,
        "suitIcon" -> card.suit.icon,
        "rank" -> card.rank.abbr
      )
    }
  }

  implicit val readsCard: Reads[Card] = (
    (JsPath \ "suit").read[Suit] and
      (JsPath \ "rank").read[Rank]
    )(Card.apply _)

  implicit val writesCardBSON = new BSONDocumentWriter[Card] {
    def write(card: Card) = BSONDocument(
      "suit" -> card.suit.string,
      "rank" -> card.rank.abbr
    )
  }

  implicit val readsCardBSON = new BSONDocumentReader[Card] {
    override def read(bson: BSONDocument): Card = Card(
      Suit.fromString(bson.getAs[String]("suit").get).get,
      Rank.fromString(bson.getAs[String]("rank").get).get
    )
  }
}