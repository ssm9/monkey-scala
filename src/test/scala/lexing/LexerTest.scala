package lexing

import org.scalatest.{FlatSpec, Matchers}

class LexerTest extends FlatSpec with Matchers{
  "Lexer" should "properly tokenize this string" in {
    val sourceCode = """let five = 5;
                       |let ten = 10;
                       |
                       |let add = fn(letx, y) {
                       |	x + y;
                       |};
                       |
                       |let result = add(five, ten);
                       |!-/*5;
                       |5 < 10 > 5;""".stripMargin
    val tokens = Lexer.tokenize(sourceCode.toList)

    tokens.foreach(t => t.isInstanceOf[Token] should be (true))
  }
}
