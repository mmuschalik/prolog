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
    for (ln <- Source.stdin.getLines) {
      val queryOption = parseQuery(ln)

      //queryOption.foreach(q => println(show(q)))

      val solution =
        for {
          p <- program
          //_ <- Some(println(p))
          query <- queryOption
          _ <- Some(println(show(query)))
          clause <- p.get(query.goals.head).headOption//next(query)(given p)
          //_ <- Some(println(clause))
          clauseRename <- Some(renameVariables(clause,1))
          _ <- Some(println(clauseRename))
          solution <- solve(query.goals.head, clauseRename.head)
          _ <- Some(println(solution))
        } yield substitute(solution, clauseRename)

      solution.foreach(s => println(s))
    }
  }
}