package reactive
package web


import net.liftweb.util.Helpers.randomString
import net.liftweb.http.{RequestVar, S}



/**
 * A RequestVar to generate a maximum of one Page instance
 * per request.
 */
object CurrentPage extends RequestVar(new Page)


/**
 * Inheriting this trait gives your Lift snippet classes
 * access to some useful functionality. It is not mandatory,
 * but you do need a Page in the implicit scope.
 */
trait ReactiveSnippet {
  private var lastPage: Option[String] = None
  /**
   * Returns the Page this snippet is rendering to.
   * Since Page extends Observing, this provides an Observing
   * in the implicit scope.
   */
  implicit def currentPage: Page =
    //TODO should the test be instead 'if(lastPage.isDefined)'?
    if(S.request.isEmpty)
      lastPage.flatMap(Reactions.findPage) getOrElse error("No page")
    else {
      val p = CurrentPage.is
      lastPage = Some(p.id)
      p
    }
  
  /**
   * Returns true if the page has not expired yet.
   */
  //TODO the comet shouldn't have to be placed above the snippet
  def isPageAlive =
    lastPage.map(Reactions.isPageAlive) getOrElse true 
    // if lastPage wasn't set yet then the page didn't even begin its lifetime and certainly
    // hasn't expired
}


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

