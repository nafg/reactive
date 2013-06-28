### Introduction

`EventStream` is the central trait in reactive-core,
in the sense that everything else is built on it. It provides a
functional abstraction over the well-known listener pattern. Rather than
using imperative techniques, however, using `EventStream` you
can send or receive events using a more effective abstraction.

It is important to note that reactive does not handle threading
implicitly. Events are delivered on whichever thread they are fired, and
execution of that thread continues when all listeners have processed the
event.

One can think of an `EventStream` as being similar to a collection of
values, except that rather than all values existing simultaneously, each one
exists at a different point in time. Keep this analogy in mind, because
methods in reactive-core are named like the corresponding methods in the scala collections framework.

#### What’s the point?

You may be wondering, why learn a new way of doing things? Why not just use the regular listener pattern, like Swing
does?

The short answer is that, while it’s a new way of thinking, in
the long run it makes life easier. For example, if you take the
imperative approach of adding a listener in response to one situation,
in one method, and removing it in response to other situations, in other
methods, the result is that you have code that manages listeners’
lifecycles sprinkled in different places in code. Of course that means
it’s harder to keep track of. By abstracting over the concept of
managing listeners’ lifecycles, it becomes possible to program less in
terms of what the computer should do next, and more in terms of what you
want to happen, resulting in more maintainable code.

However, perhaps the greater advantage, is that we have a
general-purpose abstraction called an `EventStream` that can
be transformed and composed. This will become more apparent as you read
more about `EventStream` and about everything that interacts
with it and is built on it.
