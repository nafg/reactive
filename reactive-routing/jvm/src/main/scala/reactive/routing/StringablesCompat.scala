package reactive.routing

import java.time.Instant

import scala.util.Try


trait StringablesCompat {
  implicit val instant: Stringable[Instant] = new Stringable[Instant] {
    def format = _.toString
    def parse = s => Try(Instant.parse(s)).toOption
  }
}
