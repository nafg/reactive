## Configuration

### Custom rendering of DOM mutations

By default, `reactive` updates the browser using the standard DOM API, but if you want
you can define an alternative mechanism (for instance, to use jQuery effects).

There are five DOM mutations, which are defined as case classes: 

<pre class="brush: scala">
case class InsertChildBefore(parentId: String, child: Elem, beforeId: String) extends DomMutation
case class AppendChild(parentId: String, child: Elem) extends DomMutation
case class RemoveChild(parentId: String, oldId: String) extends DomMutation
case class ReplaceChild(parentId: String, child: Elem, oldId: String) extends DomMutation
case class ReplaceAll(parentId: String, child: NodeSeq) extends DomMutation
case class UpdateProperty[T](parentId: String, propertyName: String, attributeName: String, value: T)(implicit val codec: PropertyCodec[T]) extends DomMutation
</pre>

To define a custom rendering, you need to write something like this:

<pre class="brush: scala">
implicit val config = Config(
  domMutationRenderer = CanRender {
    case InsertChildBefore(parentId, child, prevId) =&gt; ...
    case AppendChild(parentId, child) =&gt; ...
    case RemoveChild(parentId, oldId) =&gt; ...
    case ReplaceChild(parentId, child, oldId) =&gt; ...
    case ReplaceAll(parentId, child) =&gt; ...
    case up@UpdateProperty(parentId, pname, aname, v) =&gt; ...
  }
)
</pre>

You're defining a `CanRender[DomMutation]` by calling the
`CanRender` factory with a `DomMutation=&gt;String`.
The best place to put it is in your package object, so it will be in the
implicit scope of all your classes without requiring an import.
