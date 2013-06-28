### Creating an `EventStream`

Often you will be dealing with `EventStream`s
obtained from a library. However, often you to need to create one
yourself. To do this just instantiate `EventSource`
(it extends `EventStream`).
You can then send events by calling `fire`.

You also create a new `EventStream` every
time you call one of the transforming methods (below). Another kind of `EventStream` you can create is `Timer`.

 <div data-lift="DemoPane?snippet=EventStream_EventSource"></div>
