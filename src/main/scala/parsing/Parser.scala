package parsing

import lexing._
import lexing.Token._
import parsing.Node._

object Parser {

  def prefixParse(): Expression = ???

  def infixParse(expression: Expression): Expression = ???

  def parseIntegerLiteral(integer: INT): Expression = IntegerLiteral(Integer.parseInt(integer.Literal))

  def parseOperationExpression(integer: INT, operator: Token, rest: List[Token]): (Expression, List[Token]) = {
    val (right, restAfterParseExpression) = parseExpression(rest)
    (Operation(parseIntegerLiteral(integer), operator, right), restAfterParseExpression)
  }

  private def parseExpression(tokens: List[Token]): (Expression, List[Token]) = tokens match {
    case (curr: INT) :: SEMICOLON :: rest  => (parseIntegerLiteral(curr), rest)
    case (curr: INT) :: (operator @ (PLUS|MINUS)) :: rest => parseOperationExpression(curr, operator, rest)
  }

  def parseGroupedExpression() = ???

  def parseExpressionStatement(tokens: List[Token]): (ExpressionStatement, List[Token]) = {
    val (expression, rest) = parseExpression(tokens)
    (ExpressionStatement(expression), rest)
  }

  def parseLetStatement(tokens: List[Token]): (LetStatement, List[Token]) = tokens match {
    case (name: IDENT) :: ASSIGN :: rest =>
      val (expression, restAfterParseExpression) = parseExpression(rest)
      (LetStatement(Identifier(name.Literal), expression), restAfterParseExpression)
    case LPAREN :: rest => parseGroupedExpression()
  }

  def parseReturnStatement(tokens: List[Token]): (ReturnStatement, List[Token]) = {
    val (expression, rest) = parseExpression(tokens)
    (ReturnStatement(expression), rest)
  }

  def parseStatement(tokens: List[Token]): (Statement, List[Token]) = tokens match {
    case LET :: rest => parseLetStatement(rest)

    case RETURN :: rest => parseReturnStatement(rest)

    case _ => parseExpressionStatement(_)

    case EOF :: _ => (Eof, List[Token]())
  }
  def parse(tokens: List[Token]): List[Statement] = {
    def getNodes(statements: List[Statement], tokens: List[Token]): List[Statement] = parseStatement(tokens) match {
      case (Eof, _) => statements.reverse
      case (statement, rest) => getNodes(statement +: statements, rest)
    }

    getNodes(List[Statement](), tokens)
  }
}