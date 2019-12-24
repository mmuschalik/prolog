package Prolog

import zio.App
import zio.console._
import Domain._

object MyApp extends App {

  def run(args: List[String]) =
    myAppLogic.fold(_ => 1, _ => 0)

  val myAppLogic =
    for {
      program     <- compile("./src/main/resources/test.txt")
      _           <- putStrLn("Program successfully loaded.")
      _           <- putStrLn("Enter your goal:")
      queryString <- getStrLn
      query       <- parseQuery(queryString) 
    } yield next(query)(given program)

  def test(args: Array[String]): Unit = {

    //    query  <- queryOption
    //    answer <- next(query)(given p)
    //  } yield prompt(answer)(given p) 
  }
}

def prompt(result: Domain.Result)(given p: Domain.Program): Unit = {
  import Prolog.Domain.{resultShow, show}
  println(show(result))
  val line = scala.io.StdIn.readLine()
  if(line.contains(":q"))
    ()
  else {
    import Prolog.Domain._

    val opt = next(result.stack)
    opt.foreach(o => prompt(o))
  }
}