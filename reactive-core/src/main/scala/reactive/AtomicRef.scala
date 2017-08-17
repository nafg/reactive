package reactive

/**
 * A subclass of java.util.concurrent.atomic.AtomicReference[A]
 * that adds the ability to atomically modify its value relative to itself.
 */
class AtomicRef[A](init: A) extends java.util.concurrent.atomic.AtomicReference[A](init) {
  /**
   * Update the value atomically by applying a function to it.
   * Uses `compareAndSet` and will keep trying until it succeeds.
   */
  final def transform(f: A => A): A = run { a =>
    val ret = f(a)
    (ret, ret)
  }

  /**
   * Update the value atomically by applying a computation to it.
   * The computation returns a pair of values, of which the
   * `AtomicRef` is updated to the first.
   * Uses `compareAndSet` and will keep trying until it succeeds.
   * @return the second value of the computation
   */
  @annotation.tailrec final def run[B](f: A => (A, B)): B = {
    val a = get
    val (a2, b) = f(a)
    if(compareAndSet(a, a2)) b
    else {
      Thread.sleep(10)
      run(f)
    }
  }
}
