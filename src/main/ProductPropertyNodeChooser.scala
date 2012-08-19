
/*
 * The product properties node selector is responsible for matching a listing to a node in a list
 * of product properties nodes.
 * 
 * This performs an extremely simple matching algorithm.
 * 
 * Basically, we just split each property up into tokens (where each token is one word). If each of the product
 * tokens is contained within the listing tokens, we call it a match.
 * 
 * Token comparisons are done via a case insensitive string comparison.
 */
class ProductPropertyNodeChooser(private var m_listingPropertyFunction: (Listing) => TokenizableProperty) {
  
  /*
   * The word length of the tokens.
   */
  private val TOKEN_WORD_LENGTH = 1
  
  /*
   * Determine whether or not a token is contained within a list of tokens.
   */
  private def listContainsString(tokenList: List[String], value: String) : Boolean = {
    for (token <- tokenList) {
        if (token.toLowerCase().equals(value.toLowerCase())) {
          return true
        }
    }
    return false
  }
  
  /*
   * Determine if the listing's tokens match a list of product tokens.
   */
  private def doTokensMatch(productTokens: List[String], listingTokens: List[String]) : Boolean = {
    // If every token in the product tokens is contained in the listing tokens, we'll call
    // it a match
    for (productToken <- productTokens) {
      if (!listContainsString(listingTokens, productToken)) {
        return false
      }
    }
    
    // The tokens match. Note that we call it a match even though there may not be any product tokens.
    return true
  }
  
  /*
   * Match a listing to a product properties node.
   */
  def selectNode(listing: Listing, nodes: List[ProductPropertyNode]) : Option[ProductPropertyNode] = {
    var listingProperty = m_listingPropertyFunction(listing)

    for (node <- nodes) {
      if (doTokensMatch(node.property.getTokens(TOKEN_WORD_LENGTH), listingProperty.getTokens(TOKEN_WORD_LENGTH))) {
        return Some(node)
      }
    }
    
    return None
  }
}