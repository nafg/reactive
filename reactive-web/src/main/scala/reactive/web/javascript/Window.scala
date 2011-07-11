package reactive
package web
package javascript

import JsTypes._

sealed trait Window extends JsStub {
  def alert(s: $[JsString]): $[JsVoid]
  def alert(s: String): Unit = JsStatement.inScope {
    alert(s.$)
  } map (_.render) foreach Reactions.queue
  def encodeURIComponent(in: $[JsString]): $[JsString]
}
