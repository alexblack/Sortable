import java.util.Date

/*
 * Defines a known product
 */
class Product (
  val name: String,
  val manufacturer: String,
  val model: String,
  val family: String,
  val announceDate: Option[Date]) {
}

/*
 * Define a listing for a product.
 */
class Listing(val title: String,
  val manufacturer: String,
  val currency: String,
  val price: Option[Float]) {
}