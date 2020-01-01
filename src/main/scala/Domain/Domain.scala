package Prolog.Domain

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

def substitute(solution: Bindings[Term], goals: List[Goal])(given substitution: Substitution): List[Goal] = {
  solution.foldLeft(goals)((c, b) => c.map(g => substitution(g, (b._1, b._2))))
}

given Substitution = (goal, binding) => {
  new Predicate(goal.name, goal.arguments.map(a => (a, binding._1) match {
    case (v: Variable, m: Variable) if(v == m) => binding._2
    case _ => a
  }))
}

def renameVariables(predicate: Predicate, version: Int): Predicate = 
  new Predicate(predicate.name, predicate.arguments.map(a => a match {
    case Variable(name, _) => Variable(if (name.startsWith("_")) name else ("_" + name), version)
    case _ => a
  }))

def renameVariables(clause: Clause, version: Int): Clause = 
  Clause(renameVariables(clause.head, version), clause.body.map(g => renameVariables(g, version)))

def mergeBindings[T](s1: Bindings[T], s2: Bindings[T]): Bindings[T] =
  s1 ::: s2


given bindTermOrd: Ordering[Term] {
  def compare(x: Term, y: Term): Int = (x,y) match {
    case (Atom(a),Atom(b)) => a.compareTo(b)
    case (Atom(a), Variable(b, _)) => -1 
    case (Variable(a, _),Variable(b, _)) if (b.startsWith("_") && !a.startsWith("_")) => -1
    case (Variable(a, av),Variable(b, bv)) =>
      if (av == bv)
        a.compareTo(b)
      else
        av.compareTo(bv)
    case _ => 1
  }
}