package Prolog.Domain

import Term._

enum Term {
  case Variable(name: String)                           // A
  case Atom(value: String)                              // a
  case Predicate(name: String, arguments: List[Term])   // f(t1,t2,t3)
}

// Clause is of form >> H :- G1 /\ G2
case class Clause(head: Predicate, body: List[Goal]) { 
  
  def key = head.name + "_" + head.arguments.size.toString
} 

case class Query(goals: List[Goal])

case class State(query: Query, remainder: List[Clause], depth: Int)

case class Stack[T](stack: List[T]) {
  def pop: Stack[T] = if (stack.isEmpty) this else Stack(stack.tail)
  def push(t: T): Stack[T] = Stack(t :: stack)
  def peek: Option[T] = stack.headOption
}
case class Result(stack: Stack[State], solution: Option[Solution])

type Goal = Predicate
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

def collectVariables(term: Term): Set[Variable] = term match {
  case p: Predicate => p.arguments.map(m => collectVariables(m)).foldLeft(Set[Variable]())((a,b) => a union b)
  case v: Variable => Set[Variable](v)
  case _ => Set()
}


// How to show/print a Term

trait Show[T] {
  def show(t: T): String
}

given termShow: Show[Term] {   
  def show(t: Term): String = t match {
    case Variable(name) => name
    case Atom(value) => value
    case Predicate(name, args) => name + "(" + args.map(show).mkString(",") + ")"
  }
}

given queryShow: Show[Query] {
  def show(query: Query): String = query.goals.map(g => "~" +termShow.show(g)).mkString(" V ") + "."
}

def show[T](t: T)(given s: Show[T]): String = s.show(t)

