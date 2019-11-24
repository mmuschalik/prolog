package Prolog

object Main {
  import scala.meta._
  import scala.io.Source

  def main(args: Array[String]): Unit = {

    val p = parseFile("./src/main/resources/test.txt")//.parse[Term].get.structure)
    println(p)

  }
}