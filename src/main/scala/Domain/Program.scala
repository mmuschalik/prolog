package Prolog.Domain
import Term._

// Keep a map of predicate name and arity for quick lookup
case class Program private (program: Map[String, List[Clause]]) {

  def this() = this(Map())

  // add a clause to the Program
  def add(clause: Clause): Program = Program(program + (clause.key -> (clause :: program.getOrElse(clause.key, Nil))))

  // find all clauses with same predicate name and arity
  def get(goal: Predicate): List[Clause] = program.getOrElse(goal.name + "_" + goal.arguments.size, Nil)

}

// compile the file/string
def compile(file: String): Option[Predicate => Option[List[Solution]]]= 
  parseFile(file).map(p => (goal: Predicate) => 
    any(p.get(goal).map(c => solve(c)(p)(goal))).map(o => o.flatten))


def solve(clause: Clause)(p: Program): Predicate => Option[List[Solution]] = (goal: Predicate) => {
  val variables = collectVariables(goal)
  val solution = solve(clause.head, goal)

  if(falseHood(solution))
    None
  else
    Some(variables.toList.map(v => Binding(v,solution.sub(v))) :: Nil)
}

def solve(left: Term, right: Term): EqualitySet[Term] =
  EqualitySet.build(
    bind(left, right)
      .collect{ case Some(x) => x})


// given two terms, list all the bindings that would need to be happen
def bind(left: Term, right: Term): List[Option[Binding]] = (left,right) match {
  case (Atom(a), Atom(b)) if(a == b) => Nil
  case (Variable(a), Variable(b)) if(a == b) => Nil
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


def any[T](lo: List[Option[T]]): Option[List[T]] = {
  val res = lo.collect { case Some(s) => s }

  if(res.isEmpty) 
    None 
  else 
    Some(res)
}