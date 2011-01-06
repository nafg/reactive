package reactive
package web


import net.liftweb.util.Helpers.randomString
import net.liftweb.http.{RequestVar, S}


object CurrentPage extends RequestVar(new Page)

trait ReactiveSnippet {
  private var lastPage: Option[String] = None
  /**
   * Returns the Page this snippet represents.
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
  def isPageAlive =
    lastPage.map(Reactions.isPageAlive) getOrElse true 
    // if lastPage wasn't set yet then the page didn't even begin its lifetime and certainly
    // hasn't expired
}

class Page extends Observing {
  val id = randomString(20)
  def cometName = id
  def render = xml.Comment("comet " + id) ++ <lift:comet
    type="net.liftweb.reactive.ReactionsComet"
    name={cometName}
  />
}

