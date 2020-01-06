package Prolog.Domain.ADT

import Term._

enum Term derives Eql {
  case Variable(name: String, version: Integer)         // A
  case Atom(value: String)                              // a
  case Predicate(name: String, arguments: List[Term])   // f(t1,t2,t3)
}

// Clause is of form >> H :- G1 /\ G2
case class Clause(head: Goal, body: List[Goal]) { 
  
  def key = head.name + "_" + head.arguments.size.toString
} 

case class Query(goals: List[Goal])

type Goal = Predicate
type Binding[T] = (T, T)
type Bindings[T] = List[Binding[T]]

type Unification = (Goal, Goal) => Option[Bindings[Term]]
type Substitution = (Goal, Binding[Term]) => Goal