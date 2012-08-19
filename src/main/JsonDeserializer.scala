import scala.util.parsing.json.JSON
import scala.io.BufferedSource
import scala.collection.mutable.ListBuffer
import java.util.Date
import javax.xml.bind.DatatypeConverter

/*
 * Deserialize input from json.
 */
object JsonDeserializer {
  
  /*
   * Given a string, try and create a date.
   */
  private def createDate(dateStr: String) : Option[Date] = {
    try {
      var date = DatatypeConverter.parseDateTime(dateStr).getTime()
      return Some(date)
    } catch {
      case _ => return None
    }
  }
  
  /*
   * Given a string, try and convert to a float.
   */
  private def createFloat(floatStr: String) : Option[Float] = {
    try {
      return Some(floatStr.toFloat)
    } catch {
      case _ => return None
    }
  }
  
  /*
   * Deserialize a Product.
   */
  private def createProduct(json: String) : Product = {
    JSON.parseFull(json) match {
      case Some(x) => {
        var map = x.asInstanceOf[Map[String,String]]
        var family = if (map.isDefinedAt("family")) map("family") else null
        new Product(map("product_name"), map("manufacturer"),
            map("model"), family, createDate(map("announced-date")))
      }
      case None => null
    }
  }

  /*
   * Deserialize a Listing.
   */
  private def createListing(json: String) : Listing = {
    JSON.parseFull(json) match {
      case Some(x) => {
        var map = x.asInstanceOf[Map[String,String]]
        new Listing(map("title"), map("manufacturer"), map("currency"), createFloat(map("price")))
      }
      case None => null
    }
  }
 
  /*
   * Deserialize every line in the source. Create objects as we go.
   */
  private def createObjects[T](source: BufferedSource, objectCreator: (String) => T) : List[T] = {
    var objects = new ListBuffer[T]
    var lines = source.getLines()
    while(lines.hasNext) {
      var obj = objectCreator(lines.next())
      if (obj != null) objects += obj
    }
    objects.toList
  }
  
  /*
   * Create Products.
   */
  def toProducts(productsSource: BufferedSource) : List[Product]= {
    createObjects[Product](productsSource, createProduct)
  }
 
  /*
   * Create Listings.
   */
  def toListings(listingsSource: BufferedSource) : List[Listing] = {
    createObjects[Listing](listingsSource, createListing)
  }
}