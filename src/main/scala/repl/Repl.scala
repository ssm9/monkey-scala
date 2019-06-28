package repl

import scala.io.StdIn.readLine
import lexing.Lexer

object Repl {
  def main(args: Array[String]): Unit = {
    while (true) {
      val i = readLine("> ")
      val lexed = Lexer.tokenize(i.toCharArray.toList)
      println(lexed)
    }
  }
}
