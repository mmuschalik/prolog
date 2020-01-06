package Prolog.Domain.Operation

import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._

trait Show[T] {
  def show(t: T): String
}

given termShow: Show[Term]
  def show(t: Term): String = t match
    case Variable(name, version) => 
      if version == 0 then
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

given queryShow: Show[Query]
  def show(query: Query): String = 
    query
      .goals
      .map(g => "~" + termShow.show(g))
      .mkString(" V ") + "."

given resultShow: Show[Result]
  def show(result: Result): String = 
    result
      .solution
      .map(sol => 
        if sol.isEmpty then
          "true"
        else
          sol
            .sorted
            .map(a => termShow.show(a._1) + "=" + termShow.show(a._2))
            .mkString(", "))
      .getOrElse("false") //+ "\n" + 
        //stackShow.show(result.stack)

given stateShow: Show[State]
  def show(state: State): String =
    "goals:" + state.query.goals.map(q => termShow.show(q)).mkString(", ") +
      ", index: " + state.index.toString +
      ", depth: " + state.depth.toString

given stackShow: Show[Stack[State]]
  def show(stack: Stack[State]): String =
    stack.stack.map(s => stateShow.show(s)).mkString("\n")

def show[T](t: T)(given s: Show[T]): String = s.show(t)

