package Prolog.Domain.ADT
import Term._
import zio._

// Keep a map of predicate name and arity for quick lookup
case class Program private (program: Map[String, List[Clause]]) {

  def this() = this(Map())

  // add a clause to the Program
  def add(clause: Clause): Program = 
    Program(program + (clause.key -> (program.getOrElse(clause.key, Nil) :+ clause)))

  // find all clauses with same predicate name and arity
  def get(goal: Predicate): List[Clause] = 
    program.getOrElse(goal.name + "_" + goal.arguments.size, Nil)

}

