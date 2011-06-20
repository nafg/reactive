package reactive.web.demo.snippet

import net.liftweb.util._
  import Helpers._

import reactive._
  import web._
    import html._

class RepeaterDemo extends Observing {
  val signal = BufferSignal[Person]()

  def render =
    ".add *" #> Button("Add")(signal.value += Person.newPerson) &
    "#people" #> Repeater {
      signal map {
        _ map { person =>
          ".eachPerson" #> {
            ".first *" #> person.first &
              ".last *" #> person.last &
              ".delete *" #> Button("Delete") {
                signal.value -= person
              }
          }
        }
      }
    }
}

case class Person(first: String, last: String)
object Person {
  val firstNames = List("Mike", "Jack", "Jill", "John", "Joe", "Frank", "Bill", "George", "David")
  val lastNames = List("Smith", "Stone", "Johnson", "Davidson", "Jones")
  def newPerson = Person(
    firstNames(math.random * firstNames.length toInt),
    lastNames(math.random * lastNames.length toInt))
}
