package Prolog

object Main {

  import scala.meta._
  import scala.io.Source

  def main(args: Array[String]): Unit = {

    import Domain.Program._
    import Domain.Term._
    import Prolog.Domain._
    import Prolog.Domain.termShow

    val program = compile("./src/main/resources/test.txt")

    println("Enter your goal:")
    for (ln <- Source.stdin.getLines) {
      val predicate = parseQuery(ln)
      val solution =
        for {
          p <- program
          goal <- predicate
          result <- p(goal.goals.head) //fix
        } yield result

      solution.foreach(ls => {
        ls.foreach(s => println(s.map(m => show(m._1) + "=" + show(m._2)).mkString(", ")))
      })
    }
  }
}