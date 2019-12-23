package Prolog.Domain
import Term._

def next(query: Query)(given p: Program): Option[Result] = 
  next(Stack(State(query, 0, Nil, 0) :: Nil))

def next(stack: Stack[State])(given program: Program): Option[Result] = {

  for {
    state <- stack.peek
    goal <- state.query.goals.headOption
    goalRemainder = state.query.goals.tail
    clauseRemainder = 
      program
        .get(goal)
        .zipWithIndex
        .drop(state.index)

    answer <- 
      LazyList(clauseRemainder:_*)
        .map(clause => 
          for {
            clauseRename <- Some(renameVariables(clause._1, state.depth + 1))
            binding <- Prolog.Domain.Unify.unify(goal, clauseRename.head)
            
            // if no more goals left to search
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
      clauseRemainder = p.get(goal).drop(state.index + 1)
    } yield 
        if (clauseRemainder.isEmpty)
          nextState(stack.pop) // no more clauses to look at here, go back and try there
        else
          Some(stack.pop.push(State(state.query, state.index + 1, state.solution, state.depth))) // keep everything but remove a clause
    
  result.flatten
}

