package connection

import java.net.URL

import play.api.Logger
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.bson.BSONCollection
import scala.concurrent.ExecutionContext.Implicits.global
import reactivemongo.core.nodeset.Authenticate

/**
 * Created by tomas on 06-08-15.
 */

object Connection {
  def getCollection(collectionName: String): BSONCollection = {
    val driver = new MongoDriver

    val dbName = sys.env.getOrElse("PROD_MONGO_DB", "nsapi")
    val userName = sys.env.getOrElse("PROD_MONGO_USER", "")
    val password = sys.env.getOrElse("PROD_MONGO_PASS", "")
    val credentials = Seq(Authenticate(dbName, userName, password))
    val connection = driver.connection(List(sys.env.getOrElse("PROD_MONGO_HOST", "localhost")), authentications = credentials)

    val db = connection(sys.env.getOrElse("PROD_MONGO_DB", "drieen"))

    val collection = db(collectionName)
    collection
  }
}
