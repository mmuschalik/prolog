package Prolog.Domain

import scala.collection.immutable.SortedSet

// This is used to capture a set of equalities that have to be true
// It is ordered as we want to substitute with the lowest value possible
case class EqualitySet[A: Ordering] private (list: List[SortedSet[A]]) {

  // add 'a = b'
  def add(a: A, b: A): EqualitySet[A] = add(SortedSet(a, b))
  def add(t: (A, A)): EqualitySet[A] = add(t._1, t._2)

  // substitute by finding the object and returning the lowest value
  def subOption(a: A): Option[A] = 
    for {
      f <- list.find(i => i.contains(a))
      r <- f.headOption
      if(r != a)
    } yield r

  def sub(a: A): A = subOption(a).getOrElse(a)


  // helper functions
  private def add(sortedSet: SortedSet[A]): EqualitySet[A] = EqualitySet(add(list, sortedSet))

  private def merge[T](a: SortedSet[T], b: SortedSet[T]): Option[SortedSet[T]] = if(a.intersect(b).size > 0) Some(a.union(b)) else None

  private def add[T](list: List[SortedSet[T]], a: SortedSet[T]): List[SortedSet[T]] = add(list, a, false, Nil)
  private def add[T](list: List[SortedSet[T]], a: SortedSet[T], added: Boolean, result: List[SortedSet[T]]): List[SortedSet[T]] = 
    list match {
      case Nil => 
        if(added) 
          result 
        else 
          a :: result
      case h::t => 
        merge(h, a)
          .map(m => add(t, a, true, m :: result))
          .getOrElse(add(t, a, added, h :: result))
    }
}

object EqualitySet {
  def apply[A: Ordering]() = new EqualitySet(Nil: List[SortedSet[A]])
  
  def build[A: Ordering](pairs: List[(A, A)]): EqualitySet[A] = 
    pairs
      .foldLeft(new EqualitySet(Nil: List[SortedSet[A]]))((a, b) => a.add(b))
}