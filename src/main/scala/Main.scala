package Prolog

object Main {
  import scala.meta._
  import scala.io.Source

  def main(args: Array[String]): Unit = {

    import Domain.Program._
    import Domain.Term._
    import Prolog.Domain._
    import Prolog.Domain.termShow

    val solution = for {
      program <- compile("./src/main/resources/test.txt")
      goal = new Predicate("planet",List(Variable("A")))
      result <- program(goal)
    } yield result
    
    solution.foreach(ls => {
      ls.foreach(s => println(s.map(m => show(m._1) + "=" + show(m._2)).mkString(", ")))
    })

  }
}