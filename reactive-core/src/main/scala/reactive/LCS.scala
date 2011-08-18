package reactive

import scala.collection.mutable.ArrayBuffer

/**
 * Based on Java code by Neil Jones at http://bix.ucsd.edu/bioalgorithms/downloads/code/LCS.java
 */
object LCS {
  object Direction extends Enumeration {
    val Neither, Up, Left, UpAndLeft = Value
  }
  import Direction._
  def lcs[T](a: Seq[T], b: Seq[T]): Seq[T] = {
    val n = a.length
    val m = b.length

    val S = Array.ofDim[Int](n + 1, m + 1)
    val R = Array.ofDim[Direction.Value](n + 1, m + 1)

    // It is important to use to, not until.  The next two for-loops are initialization
    for (ii <- 0 to n) {
      S(ii)(0) = 0
      R(ii)(0) = Up
    }
    for (jj <- 0 to m) {
      S(0)(jj) = 0
      R(0)(jj) = Left
    }

    // This is the main dynamic programming loop that computes the score and
    // backtracking arrays.
    for (ii <- 1 to n) {
      for (jj <- 1 to m) {
        if (a(ii - 1) == b(jj - 1)) {
          S(ii)(jj) = S(ii - 1)(jj - 1) + 1
          R(ii)(jj) = UpAndLeft
        } else {
          S(ii)(jj) = S(ii - 1)(jj - 1) + 0
          R(ii)(jj) = Neither
        }

        if (S(ii - 1)(jj) >= S(ii)(jj)) {
          S(ii)(jj) = S(ii - 1)(jj)
          R(ii)(jj) = Up
        }

        if (S(ii)(jj - 1) >= S(ii)(jj)) {
          S(ii)(jj) = S(ii)(jj - 1)
          R(ii)(jj) = Left
        }
      }
    }

    // The length of the longest substring is S[n][m]
    var pos = S(n)(m) - 1

    var lcs = List[T]()

    // Trace the backtracking matrix.
    var ii = n
    var jj = m
    while (ii > 0 || jj > 0) {
      if (R(ii)(jj) == UpAndLeft) {
        ii -= 1
        jj -= 1
        lcs ::= a(ii)
      } else if (R(ii)(jj) == Up) {
        ii -= 1 // remove
      } else if (R(ii)(jj) == Left) {
        jj -= 1 // insert
      }
    }
    lcs
  }

  //TODO only compare from the first different element to the last different element
  def lcsdiff[T, U](a: Seq[T], b: Seq[U], equals: (T, U) => Boolean): Seq[SeqDelta[T, U]] = {
    val n = a.length
    val m = b.length

    val S = Array.ofDim[Int](n + 1, m + 1)
    val R = Array.ofDim[Direction.Value](n + 1, m + 1)

    // It is important to use to, not until.  The next two for-loops are initialization
    for (ii <- 0 to n) {
      S(ii)(0) = 0
      R(ii)(0) = Up
    }
    for (jj <- 0 to m) {
      S(0)(jj) = 0
      R(0)(jj) = Left
    }

    // This is the main dynamic programming loop that computes the score and
    // backtracking arrays.
    for (ii <- 1 to n) {
      for (jj <- 1 to m) {
        if (equals(a(ii - 1), b(jj - 1))) {
          S(ii)(jj) = S(ii - 1)(jj - 1) + 1
          R(ii)(jj) = UpAndLeft
        } else {
          S(ii)(jj) = S(ii - 1)(jj - 1) + 0
          R(ii)(jj) = Neither
        }

        if (S(ii - 1)(jj) >= S(ii)(jj)) {
          S(ii)(jj) = S(ii - 1)(jj)
          R(ii)(jj) = Up
        }

        if (S(ii)(jj - 1) >= S(ii)(jj)) {
          S(ii)(jj) = S(ii)(jj - 1)
          R(ii)(jj) = Left
        }
      }
    }

    // The length of the longest substring is S[n][m]
    var pos = S(n)(m) - 1

    var diffs = List[IncludeOrRemove[T, U]]()

    // Trace the backtracking matrix.
    var ii = n
    var jj = m
    while (ii > 0 || jj > 0) R(ii)(jj) match {
      case UpAndLeft =>
        ii -= 1
        jj -= 1
      case Up =>
        ii -= 1 // remove
        diffs ::= Remove(ii, a(ii))
      case Left =>
        jj -= 1 // insert
        diffs ::= Include(jj, b(jj))
    }

    var off = 0
    diffs map {
      case Include(i, e) =>
        off += 1
        Include(i, e)
      case Remove(i, e) =>
        off -= 1
        Remove(i + off + 1, e)
    }
  }
}
