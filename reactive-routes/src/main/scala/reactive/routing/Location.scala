package reactive
package routing

case class Location(path: List[String], query: List[(String, String)] = Nil) {
  def ++(xs: List[String]) = copy(path = path ++ xs)
  def :+(append: String) = copy(path = path :+ append)
  def +:(prepend: String) = copy(path = prepend +: path)
  def tail = copy(path = path.tail)
  def &(x: (String, String)) = copy(query = x :: query)
  def &&(x: (String, List[String])) = copy(query = x._2.map(x._1 -> _) ::: query)

  def takeParam(name: String): Option[(String, Location)] =
    query.span(_._1 != name) match {
      case (notIt, Nil) => None
      case (notIt, (n, v) :: rest) => Some(v -> copy(query = notIt ++ rest))
    }

  def takeParams(name: String): (List[String], Location) = {
    val (it, notIt) = query.partition(_._1 == name)
    (it.map(_._2), copy(query = notIt))
  }

  //TODO url encoding
  def toUrl = {
    val q = if(query.isEmpty) "" else "?" + query.map{case (k,v) => k+"="+v}.mkString("&")
    path.mkString("/", "/", q)
  }
}
