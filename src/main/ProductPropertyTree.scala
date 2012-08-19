import scala.collection.mutable.ListBuffer

/*
 * Companion object for static tree properties.
 */
object ProductPropertyTree {
  /*
   * The maximum depth of the product properties tree (= 3)
   * Depth = 1 -> manufacturer
   * Depth = 2 -> model
   * Depth = 2 -> family
   */
  val MAX_DEPTH = 3

  
  /*
   * Gets the product token function given a depth.
   */
  def getProductProperty(depth: Int, product: Product) : TokenizableProperty = {
    depth match {
      case 1 => return new TokenizableProperty(product.manufacturer)
      case 2 => return new TokenizableProperty(product.model)
      case 3 => return new TokenizableProperty(product.family)
      case 4 => return new TokenizableProperty("")
    }
  }
  
  /*
   * Gets the listing token function given a depth.
   */
  def getListingPropertyFunction(depth: Int) : (Listing) => TokenizableProperty = {
    depth match {
      case 1 => {listing => new TokenizableProperty(listing.manufacturer)}
      case _ => {listing => new TokenizableProperty(listing.title)}
    }
  }
}

/*
 * Represents the product property tree.
 */
class ProductPropertyTree {
  
  // The root node is special in that it doesn't represent a property.
  private var m_rootNode = new ProductPropertyNode(0,
        new TokenizableProperty(""),
        new ProductPropertyNodeChooser(ProductPropertyTree.getListingPropertyFunction(1)))
  
  /*
   * Build the tree given a list of products.
   */
  def buildTree(products: List[Product]) = {
    products.foreach(product => m_rootNode.addProduct(product))
  }
  
  /*
   * Retrieve a matching product from the tree.
   */
  def getProductMatch(listing: Listing) : Option[Product] = {
    m_rootNode.walkListing(listing)
  }
}


/*
 * A product properties node is a node in a product properties tree. Each instance
 * stores the tokens that are shared among possibly many products.
 */
class ProductPropertyNode(
    val depth: Int,  // The depth of this nod
    val property: TokenizableProperty,  // The token data of this node
    private val m_nodeSelector: ProductPropertyNodeChooser // the node selector defines how
                                                           // how to select a child for a 
                                                           // listing.
    ) {
  /*
   * The children of this node
   */
  private var m_children = new ListBuffer[ProductPropertyNode]
  
  /*
   * The product (leaf) of the tree. Only "Some" if this is the last
   * level.
   */
  private var m_product: Option[Product] = None
  
  /*
   * Determine whether or not the given product matches the properties of this node.
   */
  private def matchesProduct(product: Product) : Boolean = {
    var curProductProperty = ProductPropertyTree.getProductProperty(depth, product)
    var productTokens = curProductProperty.getTokens(1)
    var tokens = property.getTokens(1)
    
    // Doesn't match if the lists aren't the same size.
    if (tokens.size != productTokens.size) {
      return false
    }
    
    // Matches if all tokens match.
    for(i <- 0 to (tokens.size - 1)) {
      if (!tokens(i).equals(productTokens(i))) {
        return false
      }
    }
    
    // We've got a match.
    return true
  }
  
  /*
   * Finds a child for the given product.
   */
  private def findChildForProduct(product: Product) : Option[ProductPropertyNode] = {
    var childToWalk : ProductPropertyNode = null
    for (child <- children) {
      if (child.matchesProduct(product)) {
        return Some(child)
      }
    }
    return None
  }
  
  /*
   * Adds a child given a product. It is assumed that a matching node does not exist
   * at the child depth (m_depth + 1).
   */
  private def addPropertyNodeForProduct(product: Product) : ProductPropertyNode = {
    // The child depth is at a depth one greater than this nodes depth.
    var childDepth = depth + 1
    // Get the property that the new child node will represent.
    var productProperty = ProductPropertyTree.getProductProperty(childDepth, product)
    // Create the node chooser. The node chooser will choose nodes at a level below the new
    // child.
    var nodeChooser = new ProductPropertyNodeChooser(
        ProductPropertyTree.getListingPropertyFunction(childDepth + 1))
    // Finally, create the child and return it.
    var child = new ProductPropertyNode(childDepth, productProperty, nodeChooser)
    addChild(child)
    return child
  }
  
   /*
   * Adds a new child node.
   */
  private def addChild(node: ProductPropertyNode) = {
    m_children += node
  }
  
  /*
   * Gets the children nodes.
   */
  private def children : List[ProductPropertyNode] = {
    m_children.toList
  }
  
  /*
   * Sets the product
   */
  private def setProduct(product: Product) = {
    m_product match {
      case None => m_product = Some(product)
      case Some(product) => {
        // Found product with the same manufacturer, model, and family.
        // Since we can't differentiate two products and we can only have
        // one product match per listing, we'll discard both products.
        m_product = None
      }
    }
  }
  
  
  /*
   * Walk the listing down the tree. Returns a product matcher
   * if we found a product match.
   */
  def walkListing(listing: Listing) : Option[Product] = {
    // If this is the maximum depth, keep track of it as
    // a possible match.
    if (depth == ProductPropertyTree.MAX_DEPTH) {
      return m_product
    } else {
      m_nodeSelector.selectNode(listing, children) match {
        case Some(node) => {
          return node.walkListing(listing)
        }
        case None => {
          return None
        }
      }
    }
  }
  
  /*
   * Walk the tree with the given product; create nodes as we walk if the products
   * properties aren't represented in the existing tree's nodes. Finally, add the
   * product to the most specific properties node.
   */
  def addProduct(product: Product) : Unit = {
    // If we're already at the max depth. Bind the product to this node.
    if (depth == ProductPropertyTree.MAX_DEPTH) {
      setProduct(product)
    } else {
      var childToWalk : ProductPropertyNode = null
      findChildForProduct(product) match {
        // If we've found a matching child, walk it.
        case Some(childNode) => childNode.addProduct(product)
        // If we didn't find one, make a new one and walk it.
        case None => addPropertyNodeForProduct(product).addProduct(product)
      }
    }
  }
}

