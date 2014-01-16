package reactive
package routing

import java.net.URLEncoder

case class Location(path: List[String], query: List[(String, String)] = Nil) {
  /** Append path components */
  def ++(xs: List[String]) = copy(path = path ++ xs)

  /** Append a path component */
  def :+(append: String) = copy(path = path :+ append)

  /** Prepend a path component */
  def +:(prepend: String) = copy(path = prepend +: path)

  /** Drop the first path component */
  def tail = copy(path = path.tail)

  /** Add a query string parameter */
  def &(x: (String, String)) = copy(query = x :: query)

  /** Add multiple query string parameters with the same name */
  def &&(x: (String, List[String])) = copy(query = x._2.map(x._1 -> _) ::: query)

  /** Get a `Tuple2` containing the named parameter's value, and the rest of this `Location` */
  def takeParam(name: String): Option[(String, Location)] =
    query.span(_._1 != name) match {
      case (notIt, Nil) => None
      case (notIt, (n, v) :: rest) => Some(v -> copy(query = notIt ++ rest))
    }

  /** Get a `Tuple2` containing the named parameter's values, and the rest of this `Location` */
  def takeParams(name: String): (List[String], Location) = {
    val (it, notIt) = query.partition(_._1 == name)
    (it.map(_._2), copy(query = notIt))
  }

  /** Get a URL-encoded `String` representing this `Location` */
  def toUrl = {
    val enc = URLEncoder.encode(_: String, "UTF-8")
    val q = if(query.isEmpty) "" else "?" + query.map{case (k,v) => enc(k)+"="+enc(v)}.mkString("&")
    path.map(enc).mkString("/", "/", q)
  }
}
