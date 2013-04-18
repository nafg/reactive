package reactive

/**
 * Not actually a test suite; this file will simply not compile if
 * EventStreamProxy ever falls out of sync with EventStream again.
 */
class EventStreamProxyTests {

  // Asserts:
  //   * all EventStream members should be implemented by EventStreamProxy
  //   * underlying doesn't have to be public
  def proxy[A] = new EventStreamProxy[A] {
    protected[this] def underlying = sys.error("???"): EventStream[A]
  }

}
