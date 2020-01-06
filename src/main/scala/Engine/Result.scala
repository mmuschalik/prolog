package Prolog.Domain

import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._

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