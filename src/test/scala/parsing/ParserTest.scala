package parsing

import lexing.{Lexer, Token}

import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {
  "Parser" should "properly parse the tokens procuced by lexing this string" in {
    val sourceCode = """let x = 5;
                       |let y = 10;
                       |
                       |let foobar = 838383;
                       |""".stripMargin
    val tokens = Lexer.tokenize(sourceCode.toList)

    val statements = Parser.parse(tokens)

    statements.foreach(t => t.isInstanceOf[Node.Statement] should be (true))
  }
}
