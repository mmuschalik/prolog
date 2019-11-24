package Prolog.Domain

enum Term {
  case Variable(name: String)                           // A
  case Atom(value: String)                              // a
  case Predicate(name: String, arguments: List[Term])   // f(t1,t2,t3)
}

import Term._

case class Clause(head: Predicate, body: List[Term]) { 
  // H :- G1 /\ G2
  def key = head.name + "_" + head.arguments.size.toString
} 

type Binding = (Term,Term)
type Solution = List[Binding]

given bindTermOrd: Ordering[Term] {
  def compare(x: Term, y: Term): Int = (x,y) match {
    case (Atom(a),Atom(b)) => a.compareTo(b)
    case (Atom(a), Variable(b)) => -1 
    case (Variable(a),Variable(b)) => a.compareTo(b)
    case _ => 1
  }
}