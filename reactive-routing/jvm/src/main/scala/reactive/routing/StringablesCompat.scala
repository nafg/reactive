package reactive.routing

import java.time.Instant
import java.time.format.DateTimeFormatter

import scala.util.Try


trait StringablesCompat {
  implicit val instant: Stringable[Instant] = new Stringable[Instant] {
    val isoDate: DateTimeFormatter = DateTimeFormatter.ISO_DATE
    def format = _.toString
    def parse = s => Try(Instant.parse(s)).toOption
  }
}
