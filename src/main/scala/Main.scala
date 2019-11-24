package Prolog

object Main {
  import scala.meta._

  def main(args: Array[String]): Unit = {

    val p = parseProgram("f(a,b) :- g(a) && h(a) && i(a)":: Nil)//.parse[Term].get.structure)
    println(p)

  }
}