package Prolog.Domain

import Term._

enum Term {
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
type Binding = (Term, Term)
type Solution = List[Binding]

def substitute(solution: Solution, clause: Clause): Clause = {
  solution.foldLeft(clause)((c, b) => Clause(c.head, c.body.map(g => substitute(g, b._1, b._2))))
}

def substitute(goal: Goal, matchTerm: Term, subTerm: Term): Goal = {
  new Predicate(goal.name, goal.arguments.map(a => (a,matchTerm) match {
    case (Variable(vn, vv), Variable(mn, mv)) if(vn == mn && vv == mv) => subTerm
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

def mergeSolution(s1: Solution, s2: Solution): Solution =
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

def collectVariables(term: Term): Set[Variable] = term match {
  case predicate: Predicate => 
    predicate
      .arguments
      .map(m => collectVariables(m))
      .foldLeft(Set[Variable]())((a,b) => a union b)
  case v: Variable => Set[Variable](v)
  case _ => Set()
}