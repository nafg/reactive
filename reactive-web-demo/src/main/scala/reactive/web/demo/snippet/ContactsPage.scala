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
  def save = Contacts.save(this)
}

object Contacts {
  val contacts = BufferSignal[Contact]()

  def save(c: Contact) = contacts.now.indexWhere(c eq) match {
    case -1 => contacts.value += c
    case n  => contacts.value(n) = c
  }
  def delete_!(c: Contact) {
    contacts.now.indexWhere(c eq) match {
      case -1 =>
      case n  => contacts.value.remove(n)
    }
  }
}

class ContactsPage extends Observing {
  def render = "tbody" #> Repeater {
    Contacts.contacts map {
      _ map { contact =>
        val name = TextInput.value(contact.name).withEvents(DomEventSource.change) =>>
          { n => contact.copy(name = n).save }
        ".name" #> name
      }
    }
  }
}
