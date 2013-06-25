## HTML Elements

### `TextInput`

`TextInput` represents `<input
type="text"/>` HTML elements. Currently it has events
`dblClick`, `keyUp`, and `change`;
and properties `value` and `size`.

There are currently two factories: One that simply takes a
`Var[String]`; and one that takes two parameter lists,
one with the initial value as a `String` and one with
a callback as a `String=>Unit`.

<pre class="brush: scala">
val text = Var("initial")
val field1 = TextInput(text)
val field2 = TextInput(text.now)(s => text ()= s) // basically same as above

field2.value.updateOn(field2.keyUp)

field1.value.value foreach {_ => println("field1 changed")}
field2.value.value foreach {_ => println("field2 changed")}

field2.change.eventStream.foreach {_ => println("field2 change event")}
</pre>

### `CheckboxInput`

`CheckboxInput` represents `<input
type="checkbox"/>` HTML elements. Currently it has events `dblClick`,
`keyUp`, and `change`; and property `checked`.

There are currently two factories: One that simply takes a `Var[Boolean]`;
and one that takes two parameter lists, one with the initial value as a
`Boolean` and one with a callback as a `Boolean=>Unit`.

<pre class="brush: scala">
val checked = Var(false)
val check1 = CheckboxInput(checked)
val check2 = CheckboxInput(checked.now)(v => checked ()= v) // basically same as above

check1.checked.value foreach {_ => println("check1 changed")}

check2.change.eventStream.foreach {_ => println("check2 change event")}
</pre>

### `Button`

The `Button` trait represents `<button>` tags.
Override `def buttonType` to (or pass to one of the
factories) one of `ButtonType.Button`, `ButtonType.Submit`,
or `ButtonType.Reset`.

Currently, `Button` defines one event, `click`.

<pre class="brush: scala">
/**
 * Given a Button, create a Signal that holds
 * the total number of times the button has been clicked.
 */
def buttonClickCount(button: Button): Signal[Int] =
  button.click.foldLeft(0){case (n, _) => n+1}.hold(0)
</pre>

### Instantiating a `Button`

`Button` currently has four factories. All four return
a `Button with Cell`. From simplest to most complex:

####   For a `String` label, with one handler, `type="button"`:
  <pre class="brush: scala">
val button = Button("Press Me!"){
  println("I was pressed!")
}
</pre>
####   For contents defined by a `Signal[NodeSeq]`, with one handler, `type="button"`:
<pre class="brush:scala">
val imageButton =
  Button(Val(&lt;img src="/images/image.png"/>)){
    println("I was pressed")
  }
val changingTextBtn = Button(buttonText map {t => Text(t)}){
  println("I was pressed")
}
</pre>
####   For contents defined by a `Signal[NodeSeq]`, with the option to specify a button type:
<pre class="brush: scala">
val buttonBtn = Button(content = Val(Text("button"))
val submitBtn = Button(ButtonType.Submit, Val(Text("submit")))
</pre>
####   And last, is the template-friendly factory:
<pre class="brush:scala">
"button" #> Button(ButtonType.Submit, buttonAction map { action =>
  "img [src]" #> action.img &amp;
    ".text" #> action.text
})
</pre>

### `Select`

`Select` is a type-safe representation of the `<select>`
element in HTML. Currently only single-select is supported. It displays
items that are in a `SeqSignal[T]`, where `T` is a
type argument to `Select`. You can also specify a rendering
function of type `T=>String`, or use the default which is
`_.toString`. If you don't specify a value for `size`
(the height, in rows), it defaults to 1 --- a dropdown.

`Select` currently supports the `change`
event, and the `selectedIndex` property (represented as a `Option[Int]`.)
In addition, it defined `val selectedItem: Signal[Option[T]]`,
which gives you access to the selected item, if any, in terms of the
typed set of items you supplied.

Here is the current choice of factories, from simpler to more
complex:

#### Taking just a `Signal[Seq[T]]`
If the `Signal`'s value changes, a diff will be computed to determine what needs
  updating
<pre class="brush:scala">
val items = Var(List("First", "Second", "Third"))
Select(items)
</pre>

#### Taking a `Signal[Seq[T]]` (using diff algorithm) and a render function
<pre class="brush:scala">
Select(
  Val(people),
  person => person.last+", "+person.first
)
</pre>

#### Taking a `SeqSignal[T]`
<pre class="brush:scala">
val peopleSignal = BufferSignal(people: _*)
Select(peopleSignal)
</pre>

#### Taking a `SeqSignal[T]` and a rendering function.

#### Taking a current selection, a `SeqSignal[T]`, a renderer, an optional size (defaulting to 1), and a change callback
<pre
    class="brush: scala">
Select(Some(curPerson), peopleSignal, _.toString, 20){
  case None => println("Deselected!")
  case Some(person) => println("Selected " + person)
}
</pre>

#### Same as above, except taking a plain `Seq[T]` (no `Signal`), and no default value defined for `size`

 <a class="btn btn-primary" target="_blank" href="/showdemo/SelectDemo">Live Example</a>
