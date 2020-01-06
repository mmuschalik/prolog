package Prolog.Domain

import Prolog.Domain.Operation.{queryShow, show, unify, renameVariables, mergeBindings, substitute}
import Prolog.Domain.Operation._
import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._
import zio.stream.Stream

def next(query: Query)(given p: Program): Option[Result] = 
  next(Stack(State(query, 0, Nil, 1) :: Nil))

def next(stack: Stack[State])(given program: Program): Option[Result] =
  for
    state           <- stack.peek
    goal            <- state.query.goals.headOption
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
            clauseRename <- Some(renameVariables(clause._1, state.depth))
            binding      <- unify(goal, clauseRename.head)
            
            // if no more goals left to search
            // we have a result, return it
            updatedState = stack.pop.push(state.copy(index = clause._2))
            
            solution     <- if goalRemainder.isEmpty && clauseRename.body.isEmpty && binding.nonEmpty then
                              Some(Result(nextState(updatedState).getOrElse(Stack.empty), Some(mergeBindings(state.solution,binding)))) // return Result and nextState
                            else 
                              next(updatedState.push(State(Query(substitute(binding,clauseRename.body ::: goalRemainder)), 0, binding ::: state.solution, state.depth + 1))) // subst and solve next goal
          yield solution)
        .find(f => f.isDefined)
        .orElse(nextState(stack.pop).map(n => next(n)))
        .flatten
  yield answer

def nextState(stack: Stack[State])(given p: Program): Option[Stack[State]] =
  for
    state          <- stack.peek
    goal           <- state.query.goals.headOption
    clauseRemainder = p.get(goal).drop(state.index + 1)
    nextState      <- 
                      if clauseRemainder.isEmpty then 
                        nextState(stack.pop)
                      else 
                        Some(stack.pop.push(state.copy(index = state.index + 1)))
  yield nextState



def queryProgram(program: Program, query: Query): Stream[Nothing,Result] = 
  import Prolog.Domain.Operation.{resultShow, show}
  import zio.stream._

  val it = new Iterable[Result] { def iterator: Iterator[Result] = ResultIterator(program, query) }

  Stream.fromIterable(it)