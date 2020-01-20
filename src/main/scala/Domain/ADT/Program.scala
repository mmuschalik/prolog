package Prolog.Domain.ADT
import Term._
import zio._

// Keep a map of predicate name and arity for quick lookup
case class Program private (program: Map[String, List[Clause]]) {

  private def this() = this(Map())

  // add a clause to the Program
  def add(clause: Clause): Program = 
    Program(program + (clause.key -> (program.getOrElse(clause.key, Nil) :+ clause)))

  // find all clauses with same predicate name and arity
  def get(goal: Predicate): List[Clause] = 
    program.getOrElse(goal.name + "_" + goal.arguments.size, Nil)



}

object Program {
  def apply(): Program = baseProgram.foldLeft(new Program())((program, clause) => program.add(clause))

  val baseProgram: List[Clause] = 
    // eql(T,T)
    Clause(new Predicate("eql", List(Variable("T"), Variable("T")))) :: 
    // false() := eql(a,b)
    Clause(new Predicate("false", Nil), new Predicate("eql", Atom("a") :: Atom("b") :: Nil) :: Nil) ::
    // not(T) := call(T) && cut() && false
    // not(T)
    Clause(new Predicate("not", List(Variable("T"))), List(
      new Predicate("call", Variable("T") :: Nil), 
      new Predicate("cut"), 
      new Predicate("false"))) ::
    Clause(new Predicate("not", List(Variable("T"))))
    :: Nil  
    
}