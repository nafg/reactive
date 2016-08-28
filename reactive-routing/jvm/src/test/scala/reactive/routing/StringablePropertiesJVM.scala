package reactive.routing

import java.time.Instant

import org.scalacheck.Properties


object StringablePropertiesJVM extends Properties("Stringable") {
  import StringableProperties.{arbInstant, testStringable}

  property("instant") = testStringable(implicitly[Stringable[Instant]])
}
