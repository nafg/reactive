package reactive
package web
package javascript

import JsTypes._

/**
 * Defines the interface of the browser's window object.
 * See `window` in the package object -- you should use
 * that instance (you can't instantiate it directly).
 */
sealed trait Window extends JsStub {
  def alert(s: $[JsString]): $[JsVoid]
  def encodeURIComponent(in: $[JsString]): $[JsString]
}
