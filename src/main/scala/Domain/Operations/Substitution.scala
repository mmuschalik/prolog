package Prolog.Domain

import Term._

def substitute(solution: Bindings[Term], goals: List[Goal])(given substitution: Substitution): List[Goal] = {
  solution.foldLeft(goals)((c, b) => c.map(g => substitution(g, (b._1, b._2))))
}

given Substitution = (goal, binding) =>
  val variableSub = (a: Term, b: Binding[Term]) => 
    (a, b._1) match
    case (v: Variable, m: Variable) if v == m => b._2
    case _ => a

  new Predicate(goal.name, goal.arguments.map(a => variableSub(a, binding)))

def renameVariables(predicate: Predicate, version: Int): Predicate = 
  new Predicate(predicate.name, predicate.arguments.map(a => a match {
    case Variable(name, _) => Variable(if name.startsWith("_") then name else ("_" + name), version)
    case _ => a
  }))

def renameVariables(clause: Clause, version: Int): Clause = 
  Clause(renameVariables(clause.head, version), clause.body.map(g => renameVariables(g, version)))

def mergeBindings[T](s1: Bindings[T], s2: Bindings[T]): Bindings[T] =
  s1 ::: s2


given bindTermOrd: Ordering[Term] {
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
}