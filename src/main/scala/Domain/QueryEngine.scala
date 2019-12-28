package Prolog.Domain
import Term._

def next(query: Query)(given p: Program): Option[Result] = 
  next(Stack(State(query, 0, Nil, 0) :: Nil))

def next(stack: Stack[State])(given program: Program): Option[Result] = {

  for {
    state           <- stack.peek
    goal            <- state.query.goals.headOption
    _               <- Some(println(show(state.query)))
    goalRemainder    = state.query.goals.tail
    clauseRemainder  = 
      program
        .get(goal)
        .zipWithIndex
        .drop(state.index)

    answer <- 
      LazyList(clauseRemainder:_*)
        .map(clause => 
          for {
            clauseRename <- Some(renameVariables(clause._1, state.depth + 1))
            binding      <- Prolog.Domain.Unify.unify(goal, clauseRename.head)
            //_            <- Some(println(binding))
            // if no more goals left to search
            // we have a result, return it
            
            solution     <- if(goalRemainder.isEmpty && clauseRename.body.isEmpty) 
                              Some(Result(nextState(stack.pop.push(State(state.query,clause._2,state.solution, state.depth))).getOrElse(Stack.empty), Some(mergeBindings(state.solution,binding)))) // return Result and nextState
                            else 
                              next(stack.push(State(Query(substitute(binding,clauseRename.body ::: goalRemainder)), 0, binding ::: state.solution, state.depth + 1))) // subst and solve next goal
          } yield solution)
        .find(f => f.isDefined)
        .flatten
  } yield answer
}

def nextState(stack: Stack[State])(given p: Program): Option[Stack[State]] = {
  val result = 
    for {
      state          <- stack.peek
      goal           <- state.query.goals.headOption
      clauseRemainder = p.get(goal).drop(state.index + 1)
    } yield 
        if (clauseRemainder.isEmpty)
          nextState(stack.pop) // no more clauses to look at here, go back and try there
        else
          Some(stack.pop.push(State(state.query, state.index + 1, state.solution, state.depth))) // keep everything but remove a clause
    
  result.flatten
}


class MyResult(program: Program, query: Query) {
  private var result: Result = _
  private var init = false
  
  def hasNext(): Boolean = {
    val resultOption = 
      if(!init) 
        next(query)(given program)
      else 
        next(result.stack)(given program)

    resultOption
      .foreach(r => 
        {
          result = r
          init = true
        })

    resultOption.nonEmpty
  }

  def head: Result = result
}

class ResultIterator(program: Program, query: Query) extends collection.Iterator[Result] {
  val myResult = MyResult(program, query)
  var nextJustCalled = false

  def hasNext: Boolean = { 
    val res = myResult.hasNext()
    nextJustCalled = false

    res
  }

  def next(): Result = {
    if(!nextJustCalled || hasNext) {
      nextJustCalled = true
      myResult.head
    } else throw Exception("No results.")
  }
}

def queryProgram(program: Program, query: Query): ResultIterator = ResultIterator(program, query)