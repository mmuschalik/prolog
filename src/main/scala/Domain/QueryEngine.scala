package Prolog.Domain
import Term._

def next(query: Query)(given p: Program): Option[Result] = 
  next(Stack(State(query, 0, Nil, 0) :: Nil))

def next(stack: Stack[State])(given program: Program): Option[Result] = {

  for
    state           <- stack.peek
    goal            <- state.query.goals.headOption
    _               <- Some(println(show(state.query) + s" index=${state.index}"))
    goalRemainder    = state.query.goals.tail
    clauseRemainder  = 
      program
        .get(goal)
        .zipWithIndex
        .drop(state.index)

    answer <- 
      LazyList(clauseRemainder:_*)
        .map(clause => 
          for
            clauseRename <- Some(renameVariables(clause._1, state.depth + 1))
            binding      <- Prolog.Domain.Unify.unify(goal, clauseRename.head)
            //_            <- Some(println(binding))
            // if no more goals left to search
            // we have a result, return it
            
            solution     <- if goalRemainder.isEmpty && clauseRename.body.isEmpty then
                              Some(Result(nextState(stack.pop.push(State(state.query,clause._2,state.solution, state.depth))).getOrElse(Stack.empty), Some(mergeBindings(state.solution,binding)))) // return Result and nextState
                            else 
                              next(stack.push(State(Query(substitute(binding,clauseRename.body ::: goalRemainder)), 0, binding ::: state.solution, state.depth + 1))) // subst and solve next goal
          yield solution)
        .find(f => f.isDefined)
        .orElse(nextState(stack.pop).map(n => next(n)))
        .flatten
  yield answer
}

def nextState(stack: Stack[State])(given p: Program): Option[Stack[State]] =
  for
    state          <- stack.peek
    goal           <- state.query.goals.headOption
    clauseRemainder = p.get(goal).drop(state.index + 1)
    nextState      <- 
                      if clauseRemainder.isEmpty then 
                        nextState(stack.pop)
                      else 
                        Some(stack.pop.push(State(state.query, state.index + 1, state.solution, state.depth)))
  yield nextState

class MyResult(program: Program, query: Query) {
  private var result: Result = _
  private var init = false
  
  def hasNext(): Boolean = {
    val resultOption = 
      if !init then
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

  def hasNext: Boolean =
    nextJustCalled = false
    myResult.hasNext()

  def next(): Result =
    if !nextJustCalled || hasNext then
      nextJustCalled = true
      myResult.head
    else 
      throw Exception("No results.")

}

def queryProgram(program: Program, query: Query): ResultIterator = ResultIterator(program, query)