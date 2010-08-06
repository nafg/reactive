package net.liftweb.reactive

import _root_.reactive._

class Select[T](
  items: SeqSignal[T],
  renderer: T=>String = {t:T => t.toString}
) extends RParentElem {
  val size = 1
  implicit val o = new Observing {}
  val children = items.map {items: TransformedSeq[T] =>
    items.map {item: T =>
      val elem = if(selectedItem.now == Some(item))
        <option selected="selected">{renderer(item)}</option>
      else
        <option>{renderer(item)}</option>
        
      RElem(elem)
    }
  }(o)
  
  val change = new JSEventSource[Change.type]
  val selectedIndex = new JSProperty[Option[Int]] {
    val value = Var[Option[Int]](None)
    def fromString(s: String) = net.liftweb.util.Helpers.asInt(s).toOption.filter(_ != -1)
    def asString(v: Option[Int]) = v.map(_.toString) getOrElse "-1"
    def name = "selectedIndex"
    def elemId = id
    this updateOn change
  }
  val selectedItem: Signal[Option[T]] = selectedIndex.value map {optN => optN map {n=>items.now(n)}}
  def selectItem(item: Option[T]) {
    selectedIndex.value ()= item map {e=>items.now.indexOf(e)} filter(_ != -1)
  }

  def baseElem = <select size={size.toString}/>
  def properties = List(selectedIndex)
  def events = List(change)
}
object Select {
  def apply[T](selected: Option[T], items: Seq[T], renderer: T=>String, size: Int)(handleChange: Option[T]=>Unit): Select[T] =
    apply(selected, SeqSignal(Val(items))(new Observing{}), renderer, size)(handleChange)
  
  def apply[T](selected: Option[T], items: SeqSignal[T], renderer: T=>String, size: Int = 1)(handleChange: Option[T]=>Unit): Select[T] = {
    def _size = size
    new Select[T](items, renderer) {
      override val size = _size
      selectItem(selected)
      selectedItem.change foreach handleChange
    }
  }
  def apply[T](items: SeqSignal[T], renderer: T=>String): Select[T] =
    new Select[T](items, renderer)
  def apply[T](items: SeqSignal[T]): Select[T] =
    new Select[T](items)
  def apply[T](items: Signal[Seq[T]], renderer: T=>String)(implicit o: Observing): Select[T] =
    new Select[T](SeqSignal(items)(o), renderer)
  def apply[T](items: Signal[Seq[T]])(implicit o: Observing): Select[T] =
    new Select[T](SeqSignal(items)(o))
}
