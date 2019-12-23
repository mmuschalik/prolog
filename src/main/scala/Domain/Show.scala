package Prolog.Domain

import Term._

trait Show[T] {
  def show(t: T): String
}

given termShow: Show[Term] {   
  def show(t: Term): String = t match {
    case Variable(name, version) => 
      if(version == 0) 
        name 
      else 
        name + version.toString
    case Atom(value) => value
    case Predicate(name, args) => 
      name + "(" + 
        args
          .map(show)
          .mkString(",")
        + ")"
  }
}

given queryShow: Show[Query] {
  def show(query: Query): String = 
    query
      .goals
      .map(g => "~" + termShow.show(g))
      .mkString(" V ") + "."
}

given resultShow: Show[Result] {
  def show(result: Result): String = 
    result
      .solution
      .map(sol => 
        sol
          .reverse
          .map(a => termShow.show(a._1) + "=" + termShow.show(a._2))
          .mkString(", "))
      .getOrElse("false")
}

def show[T](t: T)(given s: Show[T]): String = s.show(t)

