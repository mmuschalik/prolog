package Prolog.Domain.ADT

import Term._

enum Term derives Eql {
  case Variable(name: String, version: Integer = 0)         // A
  case Atom(value: String)                              // a
  case Predicate(name: String, arguments: List[Term] = Nil)   // f(t1,t2,t3)
}

// Clause is of form >> H :- G1 /\ G2
case class Clause(head: Goal, body: List[Goal] = Nil) { 
  
  def key = head.name + "_" + head.arguments.size.toString
} 

case class Query(goals: List[Goal])

type Goal = Predicate

val TrueGoal = new Predicate("eql", Atom("a") :: Atom("a") :: Nil)
val FalseGoal = new Predicate("eql", Atom("a") :: Atom("b") :: Nil)


type Binding[T] = (T, T)
type Bindings[T] = List[Binding[T]]

type Unification = (Goal, Goal) => Option[Bindings[Term]]
type Substitution = (Goal, Binding[Term]) => Goal