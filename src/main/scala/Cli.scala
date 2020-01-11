package Prolog

import scopt.OParser
import zio.IO

case class CliConfig(
  file: String = "./application.txt",
  prompt: Boolean = false,
  top: Integer = -1
)


val builder = OParser.builder[CliConfig]
val parser1 = {
  import builder._
  OParser.sequence(
    programName("scalog"),
    head("scalog", "0.1"),
    arg[String]("<file>")
      .action((x, c) => c.copy(file = x))
      .text("file is the path to the file to compile"),
    opt[Unit]("prompt")
      .action((_, c) => c.copy(prompt = true))
      .text("for prompts between solutions"),
    opt[Int]('t', "top")
      .valueName("<n>")
      .action((x, c) => c.copy(top = x))
      .text("show a maximum of top <n> results"),
    )
}

def parse(args: List[String]) =
  IO.fromOption(OParser.parse(parser1, args, CliConfig()))