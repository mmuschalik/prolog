package Prolog

import zio.{App,Task,IO}
import zio.console._
import zio.stream.Stream

import Prolog.Domain.{compile, parseQuery}
import Prolog.Domain.Operation.{resultShow, show}
import Prolog.Domain.ADT.Result
import Prolog.Domain.queryProgram

object MyApp extends App {

  def run(args: List[String]) =
    myApp(args).fold(x => 1, _ => 0)

  val myApp = (args: List[String]) =>
    for
      config      <- parse(args)
      program     <- compile(config.file)
      _           <- putStrLn("Program successfully loaded.")
      _           <- putStrLn("Enter your goal:")
      queryString <- getStrLn
      query       <- parseQuery(queryString)
      _           <- prompt(queryProgram(program, query), config)
    yield ()
}

def prompt(stream: Stream[Nothing, Result], config: CliConfig) =
  (if(config.top > 0) then stream.take(config.top) else stream)
    .foreach(answer => 
      for 
        _   <- putStrLn(show(answer))
        str <- if config.prompt then getStrLn else IO.succeed("")
      yield ())