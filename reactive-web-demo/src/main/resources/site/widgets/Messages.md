### Messages

The `Messages` widget makes it very easy to add modeless, closable alerts to your
application. The easiest way to access it is via the application-wide
request-scoped singleton `Messages` object. That requires
you to call `Messages.init` in `Boot`. This
approach allows you to include the default template snippet,
`reactive.Messages`, in your surround template
(e.g., `templates-hidden/default.html`), outside of any other
snippet.

Alternatively you can use a local `Messages` instance.
In that case you can just insert it into the page with css selectors, e.g.,
`"#messages" #> myMessages.renderWithTemplate()`.

You can supply an alternative template for messages, either by
passing it as a `NodeSeq` to `renderWithTemplate(myTemplate)`,
or by embedding it in your own template and just calling `render`.

You can supply your own css, or use the provided css, which overlays the messages translucently,
pinned to the view with `position: fixed`.

Here is a live demonstration.

<div data-lift="DemoPane?snippet=widgets/MessagesDemo">
  <div data-lift="reactive.Messages"> </div>
  <button class="btn">Click here</button>
</div>
