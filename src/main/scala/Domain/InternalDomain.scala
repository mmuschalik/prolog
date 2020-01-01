package Prolog.Domain

import Term._

type Solve = (Query, Program) => Option[Result]

case class State(
  query: Query, 
  index: Int, 
  solution: Bindings[Term], 
  depth: Int)

case class Stack[T](stack: List[T]) {

  def pop: Stack[T] = 
    if stack.isEmpty then
      this 
    else 
      Stack(stack.tail)
  
  def push(t: T): Stack[T] = Stack(t :: stack)

  def peek: Option[T] = stack.headOption
}

object Stack {
  def empty[T]: Stack[T] = Stack(Nil)
}

case class Result(
  stack: Stack[State], 
  solution: Option[Bindings[Term]])
