package reactive.web.demo.snippet

import reactive._
import web._
import html._

import net.liftweb.util.Helpers._

case class Contact(name: String, numbers: List[String]) {
  val id = Page.currentPage.nextNumber
  def copy(name: String = this.name, numbers: List[String] = this.numbers) = new Contact(name, numbers) {
    override val id = Contact.this.id
  }
}

object Contacts {
  val contacts = BufferSignal[Contact](
    Contact("John Smith", List("124568790", "0987654321"))
  )

  def save(c: Contact) = contacts.now.indexWhere(_.id == c.id) match {
    case -1 => contacts.value += c
    case n  => contacts.value(n) = c
  }
  def delete(c: Contact) {
    contacts.now.indexWhere(_.id == c.id) match {
      case -1 =>
      case n  => contacts.value.remove(n)
    }
  }
}

class ContactsPage extends Observing {
  def render = "tbody" #> Repeater {
    Contacts.contacts.now.map{ contact =>
      val name = TextInput.value(contact.name) withEvents DomEventSource.change
      name.change =>> { n => Contacts save contact.copy(name = n) }
      val phones = BufferSignal(contact.numbers: _*)
      phones.change =>> { ps => Contacts save contact.copy(numbers = ps.toList) }
      ".name" #> name &
        ".eachPhone" #> Repeater {
          phones.now.map{ phone =>
            val input = TextInput.value(phone) withEvents DomEventSource.change
            input.change =>> { phones.value(phones.value indexOf phone) = _ }
            "input" #> input &
              ".deletephone" #> (DomEventSource.click ->> {
                phones.value.indexOf(phone) match {
                  case -1 => javascript.window.alert("Error")
                  case n  => phones.value remove n
                }
              })
          }.signal
        } &
        ".insertphone" #> (DomEventSource.click ->> { phones.value += "" }) &
        ".deletecontact" #> (DomEventSource.click ->> { Contacts delete contact })
    }.signal
  } &
    ".insertcontact" #> (DomEventSource.click ->> { Contacts save Contact("", Nil) })
}
