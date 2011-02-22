package reactive
package web
package snippet


// Transient in-memory store with method names similar to Mapper, for the time being
case class Contact(name: String, numbers: List[String]) {
  val id = Contact.nextId
  def save = Contact.save(this)
  def delete_! = Contact.delete_!(this)
  
  def copy(name: String = this.name, numbers: List[String] = this.numbers) = new Contact(name, numbers) {
    override val id = Contact.this.id
  }
}
object Contact {
  private var _nextId = 0
  private def nextId = {val ret = _nextId; _nextId += 1; ret}
  
  val contacts = BufferSignal[Contact]()
  
  def save(c: Contact) = contacts.now.indexWhere(_.id == c.id) match {
    case -1 => contacts.value += c
    case n => contacts.value(n) = c
  }
  
  def delete_!(c: Contact) {
    contacts.now.indexWhere(_.id == c.id) match {
      case -1 =>
      case n => contacts.value.remove(n)
    }
  }
  
  def findAll = contacts.now
}

class ContactsPage extends Observing {
  val curContact = Var(Contact("",Nil))
  
  val curContactNameField = TextInput(curContact.value.name){s =>
    curContact.value = curContact.value.copy(name = s)
    curContact.value.save
  }
  val contactsList = (Contact.contacts: SeqSignal[Contact]) map { _ flatMap {contact =>
    <tr><th>{contact.name}</th><td>{contact.numbers}</td></tr>: xml.NodeSeq
  }}
    
}
