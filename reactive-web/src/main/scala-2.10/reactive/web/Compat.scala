package reactive
package web

object Compat { 
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
}
