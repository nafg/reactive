## Low Level API

### Introduction

Most of the time you don’t need to think about `reactive-web`’s
internals. However, to reason about more complex situations, it may be
helpful to understand what’s happening behind the scenes. That said, the
following is not meant to paint a picture about typical usage of the
library.

At its heart, deep down, `reactive-web` is a
simple abstraction over the various ways that the server and the browser
can talk to each other. This allows you to develop powerful user interfaces
without having to be concerned with how data gets where. You don’t have to
do any more work to use comet, and you don’t have to manually add ajax calls.

This is implemented as follows. Every web page is represented by a `Page`.
Any number of `Transport`s can be installed into a `Page` at a given time.
Typically the `Transport`s available depends on the particular `Page` implementation you use.
For instance `LiftCometPage` lets you send commands to the browser
either by putting them in the document while it’s being rendered,
or by putting them in the response to a Lift ajax request,
or by sending them to a comet actor that is automatically inserted into the current page.
You receive events from the browser via Lift ajax calls.
`SsePage`, on the other hand, besides letting you put javascript in the page while
it’s being rendered, uses its own ajax system to receive events from the browser
and can put javascript commands in the response to those requests, and it can push
commands to the browser using HTML5 Server Side Events / EventSource.

To this, `reactive-web` adds primitives that represent specific browser events,
element attributes/properties, and element contents, that have FRP abstractions, based on
`reactive-core`’s FRP features.

There are also components that represent things like text inputs or select drop downs, that
conveniently combine several such primitives.

`reactive-web` objects require an implicit `Page` instance, which they use to render themselves
and receive updates from.
