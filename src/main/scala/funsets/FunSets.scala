package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {

  def main(args: Array[String]): Unit = {

  }
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (input: Int) => elem == input

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set  = (elem: Int) => s(elem) || t(elem)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (elem: Int) => s(elem) && t(elem)

  def sum(f: Int => Int, a: Int, b: Int): Int =
   if (a>b)0
   else f(a) + sum(f, a+1, b)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set  = (elem: Int) =>  s(elem) && !t(elem)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (elem: Int) => s(elem) && p(elem)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == bound) true
      else if (contains(s,a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = forall(intersect(s, p), p)

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    var set = intersect(singletonSet(-1), singletonSet(1))
    var firstIter = true

    for (i <- -bound to bound if contains(s, i)) {
      val transI = f(i)
      if (firstIter) {
        set = union(singletonSet(transI), singletonSet(transI))
      } else {
        set = union(singletonSet(transI), set)
      }
      firstIter = false
    }
    set
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
