package Prolog.Domain
import Term._

// Keep a map of predicate name and arity for quick lookup
case class Program private (program: Map[String, List[Clause]]) {

  def this() = this(Map())

  // add a clause to the Program
  def add(clause: Clause): Program = Program(program + (clause.key -> (program.getOrElse(clause.key, Nil) :+ clause)))

  // find all clauses with same predicate name and arity
  def get(goal: Predicate): List[Clause] = program.getOrElse(goal.name + "_" + goal.arguments.size, Nil)

}

// compile the file/string
def compile(file: String): Option[Program] = parseFile(file)

def next(query: Query)(given p: Program): Option[Result] = 
  for {
    g <- query.goals.headOption
    r <- next(Stack(State(query, 0, Nil, 0) :: Nil))
  } yield r

def next(stack: Stack[State])(given p: Program): Option[Result] = {

  for {
    state <- stack.peek
    //_ <- Some(println(show(state.query)))
    goal <- state.query.goals.headOption
    goalRemainder = state.query.goals.tail
    clauseRemainder = p.get(goal).zipWithIndex.drop(state.index)

    answer <- 
      LazyList(clauseRemainder:_*)
        .map(clause => 
          for {
            clauseRename <- Some(renameVariables(clause._1, state.depth+1))
            binding <- Prolog.Domain.Unify.unify(goal, clauseRename.head)
            // if no goal left and we have a solution
            // we have a result, return it
            solution <- if(goalRemainder.isEmpty && clauseRename.body.isEmpty) 
                          Some(Result(nextState(stack.pop.push(State(state.query,clause._2,state.solution, state.depth))).getOrElse(Stack.empty), Some(mergeSolution(state.solution,binding)))) // return Result and nextState
                        else 
                          next(stack.push(State(Query(substitute(binding,clauseRename).body ::: goalRemainder), 0, binding ::: state.solution, state.depth + 1))) // subst and solve next goal
          } yield solution)
        .find(f => f.isDefined)
        .flatten
  } yield answer
}

def nextState(stack: Stack[State])(given p: Program): Option[Stack[State]] = {
  val result = 
    for {
      state <- stack.peek
      goal <- state.query.goals.headOption
      clauseRemainder = p.get(goal).drop(state.index+1)
    } yield 
        if (clauseRemainder.isEmpty)
          // no more clauses to look at here, go back and try there
          nextState(stack.pop)
        else
          // keep everything but remove a clause
          Some(stack.pop.push(State(state.query, state.index + 1, state.solution, state.depth)))
    
  result.flatten
}

