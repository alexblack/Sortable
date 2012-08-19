import scala.collection.mutable.ListBuffer

/*
 * This class represents a property of a product or listing that can be
 * broken up into tokens.
 */
class TokenizableProperty(property: String) {
  
  private val SPLIT_STRING_REGEX = "[_| |-]"
  
  /*
   * Combine single word tokens into multi word tokens.
   */
  def combineTokens(wordLength: Int, tokens: List[String]) : List[String] = {
    
    // Store a list of tokens; each token is of length wordLength.
    var newTokens = new ListBuffer[String]()
    
    // Store a partially built list of single word tokens that will
    // eventually make up a wordLength token.
    var curTokenList = new ListBuffer[String]()
   
    // Build the tokens
    var tokenIterator = tokens.iterator
    while(tokenIterator.hasNext) {
      var singleWordToken = tokenIterator.next()
      if (curTokenList.size >= wordLength) {
        newTokens += curTokenList.reduceLeft((a, b) => a + " " + b)
        curTokenList.clear()
      }
      curTokenList += singleWordToken
    }
    
    // Add the last word if there is one.
    if (curTokenList.size > 0) {
      newTokens += curTokenList.reduceLeft((a, b) => a + " " + b)
    }
    
    newTokens toList
  }
    
  /*
   * Gets the tokens. Each token will be tokenWordCount long. Multi-word
   * tokens are combined with an empty string separator
   */
  def getTokens(tokenWordCount: Int) : List[String] = {
    var tempProperty = if (property != null) property else ""
    var tokens = tempProperty.split(SPLIT_STRING_REGEX)
    return combineTokens(tokenWordCount, tokens.toList)
  }

}