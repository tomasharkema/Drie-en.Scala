package user

import connection.Connection
import game.{Card, Game}
import org.joda.time.DateTime
import reactivemongo.bson.{BSONDocumentReader, BSONDocumentWriter, BSONDocument}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by tomas on 10-08-15.
 */

case class User(id: String,
                name: String)

object User {

  val userCollection = Connection.getCollection("users")

  def getUser(id: String): Future[Option[User]] = userCollection
    .find(BSONDocument("id" -> id))
    .cursor[User].collect[List]()
    .map(_.headOption)

  def getUserByName(name: String): Future[Option[User]] = userCollection
    .find(BSONDocument("name" -> name))
    .cursor[User].collect[List]()
    .map(_.headOption)

  def getUserByIds(ids: Seq[String]): Future[Seq[User]] = userCollection
    .find(BSONDocument("$or" -> ids.map(id => BSONDocument("id" -> id))))
    .cursor[User].collect[List]()

  implicit val writesUserBSON = new BSONDocumentWriter[User] {
    def write(user: User) = BSONDocument(
      "id" -> user.id,
      "name" -> user.name
    )
  }

  implicit val readsUserBSON = new BSONDocumentReader[User] {
    override def read(bson: BSONDocument): User = User(
      bson.getAs[String]("id").get,
      bson.getAs[String]("name").get
    )
  }
}