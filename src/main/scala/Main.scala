package Prolog

import zio.{App,Task}
import zio.console._
import Domain._

object MyApp extends App {

  def run(args: List[String]) =
    myApp.fold(_ => 1, _ => 0)

  val myApp =
    for
      program     <- compile("./src/main/resources/test.txt")
      _           <- putStrLn("Program successfully loaded.")
      _           <- putStrLn("Enter your goal:")
      queryString <- getStrLn
      query       <- parseQuery(queryString)
      _           <- prompt(Domain.queryProgram(program, query))
    yield ()

}

def prompt(resultIterator: ResultIterator) = {
  import Prolog.Domain.{resultShow, show}
  import zio.stream._

  val it = new Iterable[Result] { def iterator: Iterator[Result] = resultIterator }

  Stream
    .fromIterable(it)
    .foreach(r => 
      for 
        _   <- putStrLn(show(r))
        //str <- getStrLn
      yield ())
}