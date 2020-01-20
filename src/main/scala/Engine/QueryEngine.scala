package Prolog.Domain

import Prolog.Domain.Operation.{stackShow, queryShow, show, unify, renameVariables, mergeBindings, substitute}
import Prolog.Domain.Operation._
import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._
import zio.stream.Stream

def getGoal(goal: Goal): Goal =
  goal match
  case Predicate("call", (t: Predicate) :: Nil) => getGoal(t)
  case _ => goal

def next(query: Query)(given p: Program): Option[Result] = 
  next(Stack(State(query, 0, Nil, 1) :: Nil))

def next(stack: Stack[State])(given program: Program): Option[Result] =
  val stackWithCut = stack // cutStack(stack)
  
  for
    state           <- stackWithCut.peek
    goal            <- state.query.goals.headOption.map(getGoal)
    goalRemainder    = state.query.goals.tail
    clauseRemainder  = 
      program
        .get(goal)
        .zipWithIndex
        .drop(state.index)

    answer <- 
      if goalIsCut(goal) then
        Some(Result(stack.pop.push(state.copy(query = Query(goalRemainder))), Some(state.solution), true))
      else
        LazyList(clauseRemainder:_*)
          .map(clause => 
            for
              clauseRename <- Some(renameVariables(clause._1, state.depth))
              binding      <- unify(goal, clauseRename.head)
              
              // if no more goals left to search
              // we have a result, return it
              updatedState = stackWithCut.pop.push(state.copy(index = clause._2))
              
              solution     <- if goalRemainder.isEmpty && clauseRename.body.isEmpty && binding.nonEmpty then
                                Some(Result(nextState(updatedState).getOrElse(Stack.empty), Some(mergeBindings(state.solution,binding)))) // return Result and nextState
                              else 
                                next(updatedState.push(State(Query(substitute(binding,clauseRename.body ::: goalRemainder)), 0, binding ::: state.solution, state.depth + 1))) // subst and solve next goal
            yield (solution, clauseRename.body.find(c => goalIsCut(c)).isDefined))
          .find(f => f.isDefined)
          .map(m =>
            for
              pair <- m
              ret  <- if pair._1.isCut && pair._2 then 
                        // everything should have been cut, so continue solving now
                        //println("caught it!")
                        val newStack = pair._1.stack.peek.map(p => pair._1.stack.pop.pop.push(p))
                        //println(show(newStack.get))
                        //scala.io.StdIn.readChar
                        next(newStack.getOrElse(Stack.empty)) // dont explore alternate solutions for the clause head 
                      else if pair._1.isCut then
                        // we need to cut, but more to be cut...
                        //println("cutting...")
                        Some(Result(pair._1.stack.peek.map(p => pair._1.stack.pop.pop.push(p)).getOrElse(Stack.empty), pair._1.solution, true))
                      else
                        // no cut, just return the solution
                        Some(pair._1)
            yield ret)
          .orElse(nextState(stackWithCut.pop).map(n => next(n)))
          .flatten
  yield answer

def goalIsCut(goal: Goal): Boolean = 
  goal match
  case Predicate("cut", Nil) => true
  case _ => false   

def nextState(stack: Stack[State])(given p: Program): Option[Stack[State]] =
  for
    state          <- stack.peek
    goal           <- state.query.goals.headOption
    clauseRemainder = p.get(getGoal(goal)).drop(state.index + 1)
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