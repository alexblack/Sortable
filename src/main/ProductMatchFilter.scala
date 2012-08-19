import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

/*
 * Static class to convert foreign currencies to CAD
 */
object CurrencyConverter {
  
  private val m_currencyMap = new HashMap[String, Double]()
  
  /*
   * Known currencies.
   */
  m_currencyMap += ("EUR" -> 1.22361)
  m_currencyMap += ("USD" -> 0.992594)
  m_currencyMap += ("GBP" -> 1.55627)
  m_currencyMap += ("INR" -> 0.0179352)
  m_currencyMap += ("AUD" -> 1.04357)
  m_currencyMap += ("SGD" -> 0.796451)
  m_currencyMap += ("CAD" -> 1)
  
  /*
   * Converts the given dollar amount in the given currency to CAD.
   * If the currency is unknown, abandon the conversion.
   * 
   * Returns Some[Float] if conversion successful, None otherwise.
   */
  def getCadValue(dollarValue: Float, currency: String) : Option[Float] = {
    if (m_currencyMap.contains(currency)) {
      return Some(m_currencyMap(currency).asInstanceOf[Float] * dollarValue)
    }
    return None
  }
  
}

/*
 * The product match filter is given listings that have already been screened and
 * identified as potential matches. This class examines the potential matches in the
 * context of the other potential matches.
 */
class ProductMatchFilter(val product: Product) {
  
  /*
   * If a value is greater or less than the average price by the outlier
   * rejection percentage then we discard it.
   */
  private val OUTLIER_REJECTION_PERCENTAGE = .3
  
  /*
   * The listings that have been identified as possible matches for this product.
   */
  private var m_listings = new ListBuffer[Listing]()
  
  /*
   * The running average price for the product (according to the listings)
   */
  private var m_averagePrice : Float = 0
  
  
  /*
   * Determine whether or not the given listing has a price mismatch.
   */
  private def hasPriceMismatch(listing: Listing) : Boolean = {
    var buffer = m_averagePrice * OUTLIER_REJECTION_PERCENTAGE
    val min = m_averagePrice - buffer
    val max = m_averagePrice + buffer
    val cadValue = CurrencyConverter.getCadValue(listing.price.get, listing.currency).get
    return cadValue < min || cadValue > max
  }
  
  /*
   * Add a listing to the product match filter. Under certain circumstances,
   * the listing may be rejected outright.
   */
  def addListing(listing: Listing) : Unit = {
    listing.price match {
      // Don't add the listing if we don't have a price..
      case None => {
        return
      }
      case Some(price) => {
        CurrencyConverter.getCadValue(price, listing.currency) match {
          // If the price can be converted to CAD, add it and update the
          // average price
          case Some(cadPrice) => {
            var total = m_averagePrice * m_listings.size.asInstanceOf[Float]
            m_averagePrice = (total + cadPrice) / (m_listings.size.asInstanceOf[Float] + 1.0f)
            m_listings += listing
          }
          // Don't add if we couldn't convert.
          case None => return
        }
      }
    }
  }
  
  /*
   * Gets all the listings.
   */
  def listings = m_listings.toList
  
  /*
   * Remove the listings that are either too good to be true or too bad to be true.
   */
  def filter() {
    var newListings = new ListBuffer[Listing]()
    for (listing <- listings) {
      if (!hasPriceMismatch(listing)) {
        newListings += listing
      }
    }
    
    m_listings = newListings
  }
} 