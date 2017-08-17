## Logger: Semantic logging

In order to allow for fine-grained configurable logging without making any assumptions about the logging backend,
`reactive` takes the following approach. Rather than logging directly, any information that can be logged
has a case class defined, containing the relevant details. When logging for that object is at a high enough level,
this information will be put in a LogEvent and fired from a global EventStream. LogEvent is a case class with two
arguments: a subject, i.e. the object "logging" the information, and a predicate, the first case class we mentioned.

The `Logger` trait supplies the ability to log. It’s used by `reactive` classes, and you
can use it in your own classes. Either mix it in, or instantiate it, optionally overriding `subject`
(presumably to point to the containing object). It has the following members:

* #### `logLevel`
A `var` specifying the logging level.

* ####`setLogLevel`
Sets the log level and returns the `Logger`. Useful for chaining
`reactive` operations. For instance:

 <pre class="brush:scala">val es2 = es.setLogLevel(Logger.Levels.Trace).map(_ + 2)</pre>

* ####`log`
Logs a `LogEvent` based on the specified level and `LogEventPredicate`.</dd>

* ####`trace`, `warn`, `error`
Logs a `LogEvent` based on the specified
`LogEventPredicate`, at the relevant level.

The `Logger` singleton contains the `EventStream`(s)
that fire the log events. Here are some of its members:

* ####`Levels`
  A Scala `Enumeration`
of the supported levels.

* ####`defaultLevel`
A `var`
specifying the level of subsequently created `Logger`s.

* ####`all`
An `EventStream[(Levels.Value,Logger#LogEvent)]` that
fires all events.

* ####`traces`, `warnings`, `errors`
`EventStream[Logger#LogEvent]`s firing only the
relevant events from `all`.
