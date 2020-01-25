package parsing

import lexing._
import lexing.Token._
import parsing.Node._

object Parser {

  private def parseExpressions(tokens: List[Token]): (Expression, List[Token]) = tokens match {
    case (curr: INT) :: rest  => (Identifier(curr), rest)
  }

  def parseIntegerLiteral(integer: INT): Expression = {
    IntegerLiteral(integer, Integer.parseInt(integer.Literal))
  }

  def parseOperationExpression(integer: INT, operator: Token, rest: List[Token]): (Node, List[Token]) = {
    val (right, restAfterParseExpression) = parseExpressions(rest)
    (Operation(parseIntegerLiteral(integer), operator, right), restAfterParseExpression)
  }

  def parseGroupedExpression(): (Node, List[Token]) = ???

  def parseLetStatement(tokens: List[Token]): (Node, List[Token]) = tokens match {
    case (name: IDENT) :: ASSIGN :: (curr: INT) :: SEMICOLON :: rest => (parseIntegerLiteral(curr), rest)
    case (curr: INT) :: (operator @ (PLUS|MINUS)) :: rest => parseOperationExpression(curr, operator, rest)
    case LPAREN :: rest => parseGroupedExpression()
  }

  private def parseStatements(tokens: List[Token]): (Node, List[Token]) = tokens match {
    case LET :: rest => parseLetStatement(rest)
    case _ => ???
  }
  def parse(tokens: List[Token]): Unit = {

  }
}