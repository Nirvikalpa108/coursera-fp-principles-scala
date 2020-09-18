package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  type FunSet = Int => Boolean

  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): FunSet = {
    i: Int => i == elem
  }

  def union(s: FunSet, t: FunSet): FunSet = {
    i: Int => s(i) || t(i)
  }
  def intersect(s: FunSet, t: FunSet): FunSet = {
    i: Int => s(i) && t(i)
  }
  def diff(s: FunSet, t: FunSet): FunSet = {
    i:Int => s(i) && !t(i)
  }
  // select only the elements of the set that are accepted by
  // the given predicate.
  // the filtered elements are returned as a new set
  def filter(s: FunSet, p: Int => Boolean): FunSet = {
    i: Int => s(i) && p(i)
  }

  // if a is less than -1000 or more than 1000, return false
  // if a is not present in the set, return false
  // if the predicate is false for a, return false
  // otherwise, iter should return true
  // then, do this again for the next integer

  /** all numbers in the set satisfy the predicate */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    /** a and all numbers above a that are in the set satisfy the predicate */
    def iter(a: Int): Boolean = {
      if (a > 1000) false
      else if (!s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-1000)
  }

  // Using forall, test whether a set contains at least one element for which the given predicate is true.
  def exists(s: FunSet, p: Int => Boolean): Boolean = ???

  // using forall or exists, write a function map which transforms a given set into another one
  // by applying to each of its elements the given function
  def map(s: FunSet, f: Int => Int): FunSet = ???
}

