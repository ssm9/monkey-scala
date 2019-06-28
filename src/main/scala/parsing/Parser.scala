package parsing

import lexing._
import lexing.Token._
import parsing.Node._

object Parser {

  def parseLetExpression(rest: List[Token]): (Node, List[Token]) = rest match {
    case (curr: IDENT) :: EQ :: ??? =>

    ???
  }

  private def parseExpressions(tokens: List[Token]): (Node, List[Token]) = tokens match {
    case (curr: IDENT) :: rest  => (Identifier(curr), rest)
    case LET :: rest => parseLetExpression(rest)
  }

  def parse(tokens: List[Token]): Unit = {

  }
}