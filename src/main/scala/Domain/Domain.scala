package Prolog.Domain

enum Term {
  case Variable(name: String)                           // A
  case Atom(value: String)                              // a
  case Predicate(name: String, arguments: List[Term])   // f(t1,t2,t3)
}

import Term._

case class Clause(head: Predicate, body: List[Goal]) // H :- G1 /\ G2 

type Program = List[Clause]
type Bindings = List[Binding]
type Goal = Term

type Binding = (Variable,Term)
type Substitution = (Variable, Variable)
type Solution = List[Binding]
