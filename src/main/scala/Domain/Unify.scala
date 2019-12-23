package Prolog.Domain.Unify
import Prolog.Domain._
import Prolog.Domain.Term._
import Prolog.Domain.bindTermOrd

def unify(queryGoal: Goal, clauseGoal: Goal): Option[Solution] = {
  import Prolog.Domain.bindTermOrd
  val s = solveTerm(queryGoal, clauseGoal)
  if(falseHood(s))
    None
  else
    Some(s.list
      .flatMap(l => l.collect{ case v: Variable => v: Term })
      .flatMap(m => s.subOption(m).map(o => (m,o))))
}


def solveTerm(left: Term, right: Term): EqualitySet[Term] =
  EqualitySet.build(
    bind(left, right)
      .collect{ case Some(x) => x})


// given two terms, list all the bindings that would need to be happen
def bind(left: Term, right: Term): List[Option[Binding]] = (left,right) match {
  case (Atom(a), Atom(b)) if(a == b) => Nil
  case (Variable(an,av), Variable(bn,bv)) if(an == bn && av == bv) => Nil
  case (a: Variable, b: Variable) => Some(Binding(a,b)) :: Nil
  case (a: Variable, b: Atom) => Some(Binding(a, b)) :: Nil
  case (a: Atom, b: Variable) => Some(Binding(b, a)) :: Nil
  case (pa: Predicate, pb: Predicate) if(pa.name == pb.name && pa.arguments.size == pb.arguments.size) => 
    (pa.arguments zip pb.arguments)
      .flatMap(m => bind(m._1,m._2))
  case (a,b) => Some((a,b)) :: Nil
}

// any set with two (different) atoms can't be true
def falseHood(es: EqualitySet[Term]): Boolean = 
  es.list
  .find(f => 
    (f.headOption, f.drop(1).headOption) 
      match { 
        case (Some(a: Atom), Some(b: Atom)) => true
        case _ => false
      })
  .isDefined