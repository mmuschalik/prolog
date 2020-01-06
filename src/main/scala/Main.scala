package Prolog

import zio.{App,Task}
import zio.console._
import zio.stream.Stream

import Prolog.Domain.{compile, parseQuery}
import Prolog.Domain.Operation.{resultShow, show}
import Prolog.Domain.ADT.Result

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

def prompt(stream: Stream[Nothing, Result]) =
  stream
    .foreach(r => 
      for 
        _   <- putStrLn(show(r))
        str <- getStrLn
      yield ())