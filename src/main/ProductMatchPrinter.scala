import java.io.File
import com.google.gson.Gson
import com.google.gson.stream.JsonWriter
import java.io.PrintWriter
import java.io.FileWriter

/*
 * Defines behaviour for printing product matches.
 */
trait ProductMatchPrinter {

  /*
   * Given a product and a list of listings, print them.
   */
  def printMatches(product: Product, listings: List[Listing])
}

/*
 * Prints product matches in Json format.
 */
class JsonProductMatchPrinter(private val m_fileName: String) extends ProductMatchPrinter {
  
  // Create the output stream.
    var file = new File(m_fileName)
    private var m_out = new FileWriter(file)
  
  /*
   * Print the product matches.
   */
  def printMatches(product: Product, listings: List[Listing]) : Unit = {
    
    // If there are no matches, don't print anything.
    if (listings.size == 0)
      return
 
    var jsonWriter = new JsonWriter(m_out)
    jsonWriter.beginObject()
    jsonWriter.name("product_name").value(product.name)
    
    jsonWriter.name("listings")
    jsonWriter.beginArray()
    
    for(listing <- listings) {
      jsonWriter.beginObject()
      jsonWriter.name("title").value(listing.title)
      jsonWriter.name("manufacturer").value(listing.manufacturer)
      jsonWriter.name("currency").value(listing.currency)
      jsonWriter.name("price").value(listing.price.get.toString())
      jsonWriter.endObject()
    }
    
    jsonWriter.endArray()
    jsonWriter.endObject()
    
    jsonWriter.flush()
    m_out.append('\r')
    m_out.append('\n')
  }
    
  def flush() {
    m_out.close()
  }
}