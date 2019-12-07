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

def collect[T <: Term](term: Term): Set[T] = term match {
  case p: Predicate => p.arguments.map(m => collect[T](m)).foldLeft(Set[T]())((a,b) => a union b)
  case v: T => Set[T](v)
  case _ => Set()
}




trait Show[T] {
  def show(t: T): String
}

given termShow: Show[Term] {   
  def show(t: Term): String = t match {
    case Variable(name) => name
    case Atom(value) => value
    case Predicate(name, _) => name
  }
}

def show[T](t: T)(given s: Show[T]): String = s.show(t)

