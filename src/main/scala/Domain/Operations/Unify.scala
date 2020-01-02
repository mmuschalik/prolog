package Prolog.Domain.Unify
import Prolog.Domain._
import Prolog.Domain.Term._
import Prolog.Domain.bindTermOrd

def unify: Unification = (queryGoal, clauseGoal) => 
  val equalitySet = solveTerm(queryGoal, clauseGoal)

  Option.when(valid(equalitySet))(
      equalitySet
        .list
        .flatMap(sortedSet => sortedSet.collect { case v: Variable => v: Term })
        .flatMap(term => 
          equalitySet
            .subOption(term)
            .map(subTerm => (term, subTerm))))

def solveTerm(left: Term, right: Term): EqualitySet[Term] =
  EqualitySet.build(
    bind(left, right)
      .collect{ case Some(x) => x })


// given two terms, list all the bindings that would need to be happen
def bind(left: Term, right: Term): List[Option[Binding[Term]]] = 
  (left, right) match
  case (Atom(a), Atom(b)) if a == b => Nil
  case (a: Variable, b: Variable) if a == b => Nil
  case (a: Variable, b: Variable) => Some(Binding[Term](a, b)) :: Nil
  case (a: Variable, b: Atom) => Some(Binding[Term](a, b)) :: Nil
  case (a: Atom, b: Variable) => Some(Binding[Term](b, a)) :: Nil
  case (pa: Predicate, pb: Predicate) 
    if pa.name == pb.name && pa.arguments.size == pb.arguments.size => 
      (pa.arguments zip pb.arguments)
        .flatMap(m => bind(m._1, m._2))
  case (a, b) => Some((a, b)) :: Nil

// any set with two (different) atoms can't be true
def valid(equalitySet: EqualitySet[Term]): Boolean = 
  equalitySet
    .list
    .find(f => 
      (f.headOption, f.drop(1).headOption) 
        match { 
          case (Some(a: Atom), Some(b: Atom)) => true
          case _ => false
        })
    .isEmpty