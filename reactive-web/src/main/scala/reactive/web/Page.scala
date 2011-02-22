package reactive
package web


import net.liftweb.util.Helpers.randomString
import net.liftweb.http.{RequestVar, S}



/**
 * A RequestVar to generate a maximum of one Page instance
 * per request.
 * The value is automatically used when an implicit Page
 * is required while a request is being processed.
 */
object CurrentPage extends RequestVar(new Page)

/**
 * A Page uniquely identifies a web page rendered with reactive-web components.
 * It is used to associate RElems and ReactionsComets.
 * An RElem can be associated with multiple Pages. The corresponding
 * element will be kept in sync in both places.
 */
class Page extends Observing {
  val id = randomString(20)
  def cometName = id
  def render = xml.Comment("comet " + id) ++ <lift:comet
    type="net.liftweb.reactive.ReactionsComet"
    name={cometName}
  />
}

object Page {
  /**
   * Makes the Page corresponding the current request available implicitly.
   * Must be called when S.request.isDefined
   */
  implicit def currentPage: Page = {
    require(S.request.isDefined, "no current request, page undefined")
    CurrentPage.is
  }
}

