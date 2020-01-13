package Prolog.Domain

import zio._
import scala.io.Source
import Prolog.Domain.ADT.Program

// compile the file/string
def compile(filePath: String): Task[Program] = 
  for
    lines    <- IO.effect(
                  Source
                    .fromFile(filePath)
                    .getLines
                    .filter(f => f.trim.nonEmpty)
                    .toList)
    program  <- IO.fromOption(parseProgram(lines))
                  .mapError(er => Exception("Can't parse program."))
  yield program
