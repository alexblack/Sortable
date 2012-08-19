import scala.collection.mutable.HashMap

/*
 * A structure to store all the product matches.
 */
class ProductMatcher {
  
  /*
   * Maps product names to product match filters.
   */
  private var m_productMatches = new HashMap[String, ProductMatchFilter]()
  
  /*
   * Adds a listing match for a product.
   */
  def addMatch(product: Product, listing: Listing) = {
    if (!m_productMatches.contains(product.name)) {
      m_productMatches += (product.name -> new ProductMatchFilter(product))
    }
    m_productMatches(product.name).addListing(listing)
  }
  
  /*
   * In light of all the matches that exist for each product, filter
   * out the ones that don't make sense.
   */
  def filterOutPoorMatches() = {
    m_productMatches.values.foreach(matcher => matcher.filter())
  }
  
  /*
   * Print the product matches using the given printer.
   */
  def print(printer: ProductMatchPrinter) = {
    m_productMatches.keys.foreach(productName =>
      printer.printMatches(m_productMatches(productName).product,
          m_productMatches(productName).listings))
  }

}