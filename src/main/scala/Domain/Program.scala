package Prolog.Domain

case class Program private (program: Map[String, List[Clause]]) {

  def this() = this(Map())

  def add(clause: Clause): Program = Program(program + (clause.key -> (clause :: program.getOrElse(clause.key, Nil))))

  def get(goal: Term.Predicate): List[Clause] = program.getOrElse(goal.name + "_" + goal.arguments.size, Nil)
}