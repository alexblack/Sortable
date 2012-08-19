import scala.collection.mutable.HashMap

object Main {
  
  val PRODUCTS_INDEX = 0
  val LISTINGS_INDEX = 1
  val RESULTS_INDEX = 2
  
  def main(args: Array[String]) {
    
    // Deserialize the products and listings.
	var productJsonFile = io.Source.fromFile(args(PRODUCTS_INDEX))
    var products = JsonDeserializer.toProducts(productJsonFile)
	
	var listingsJsonFile = io.Source.fromFile(args(LISTINGS_INDEX))
    var listings = JsonDeserializer.toListings(listingsJsonFile)
    
    // Build the product property tree.
    var tree = new ProductPropertyTree()
    tree.buildTree(products)
    
    // Loop through the listings and try and match each to a product
    // in the tree.
    var productMatcher = new ProductMatcher()
    for(listing <- listings) {
      var matchingProduct = tree.getProductMatch(listing)
      matchingProduct match {
        
        // If we've found a match, add it to the product matcher.
        case Some(product) => {
          productMatcher.addMatch(product, listing)
        }
        case None => {}
      }
    }
    // Filter out the poor matches.
    productMatcher.filterOutPoorMatches()
    
    // Print the matches using the json printer.
    var printer = new JsonProductMatchPrinter(args(RESULTS_INDEX))
    productMatcher.print(printer)
    printer.flush()
  }
}

