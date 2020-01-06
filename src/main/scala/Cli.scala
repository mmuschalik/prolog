package Prolog

case class CliConfig(
  file: String = "./application.txt",
  prompt: Boolean = false,
  top: Integer = -1
)

import scopt.OParser
val builder = OParser.builder[CliConfig]
val parser1 = {
  import builder._
  OParser.sequence(
    programName("scalog"),
    head("scalog", "0.1"),
    opt[String]('f', "file")
      .action((x, c) => c.copy(file = x))
      .text("file is the path to the file to compile"),
    opt[Boolean]('p', "prompt")
      .action((x, c) => c.copy(prompt = x))
      .text("set prompt to true for prompts between solutions"),
    opt[Int]('t', "top")
      .action((x, c) => c.copy(top = x))
      .text("show a maximum of top n results"),
    )
}

import zio._

def parse(args: List[String]) =
  IO.fromOption(OParser.parse(parser1, args, CliConfig()))