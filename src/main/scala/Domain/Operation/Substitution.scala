package Prolog.Domain.Operation

import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._

def substitute(solution: Bindings[Term], goals: List[Goal]): List[Goal] =
  solution.foldLeft(goals)((c, b) => c.map(g => substitution(g, (b._1, b._2))))

def substitution(a: Goal, b: Binding[Term]): Goal =
    substiteTerm(a,b) match
    case p: Predicate => p
    case _ => a

def substiteTerm(a: Term, b: Binding[Term]): Term =
  (a, b._1) match
  case (v: Variable, m: Variable) if v == m => b._2
  case (p: Predicate, _) => new Predicate(p.name, p.arguments.map(s => substiteTerm(s, b)))
  case _ => a

def renameVariables(predicate: Predicate, version: Int): Predicate = 
  new Predicate(predicate.name, predicate.arguments.map(a => a match {
    case Variable(name, _) => Variable(if name.startsWith("_") then name else ("_" + name), version)
    case p: Predicate => renameVariables(p, version)
    case _ => a
  }))

def renameVariables(clause: Clause, version: Int): Clause = 
  Clause(renameVariables(clause.head, version), clause.body.map(g => renameVariables(g, version)))

def mergeBindings[T](s1: Bindings[T], s2: Bindings[T]): Bindings[T] =
  s1 ::: s2


given bindTermOrd: Ordering[Term]
  def compare(x: Term, y: Term): Int = 
    (x,y) match
    case (Atom(a),Atom(b)) => a.compareTo(b)
    case (Atom(a), Variable(b, _)) => -1 
    case (Variable(a, _),Variable(b, _)) if b.startsWith("_") && !a.startsWith("_") => -1
    case (Variable(a, av),Variable(b, bv)) =>
      if av == bv then
        a.compareTo(b)
      else
        av.compareTo(bv)
    case _ => 1