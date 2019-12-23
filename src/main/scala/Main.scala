package Prolog

object Main {

  import scala.meta._
  import scala.io.Source

  def main(args: Array[String]): Unit = {

    import Domain.Program._
    import Domain.Term._
    import Prolog.Domain._
    import Prolog.Domain.termShow
    import Prolog.Domain.queryShow

    val program = compile("./src/main/resources/test.txt")

    println("Enter your goal:")
    val ln = scala.io.StdIn.readLine()
    val queryOption = parseQuery(ln)

    //queryOption.foreach(q => println(show(q)))

    val solution =
      for {
        p <- program
        //_ <- Some(println(p))
        query <- queryOption
        answer <- next(query)(given p)
      } yield prompt(answer)(given p) 
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