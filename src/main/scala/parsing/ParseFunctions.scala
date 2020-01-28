package parsing

import lexing.Token
import lexing.Token.{ASSIGN, BANG, IDENT, INT, LPAREN, MINUS, PLUS, SEMICOLON}
import parsing.Node.{Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Operation, PrefixExpression, ReturnStatement}

class ParseFunctions {

  def prefixParse(): Expression = ???

  def infixParse(expression: Expression): Expression = ???

  def parseIdentifier(token: IDENT): Expression = Identifier(token.Literal)

  def parseIntegerLiteral(integer: INT): Expression = IntegerLiteral(Integer.parseInt(integer.Literal))

  def parsePrefixExpression(prefixToken: Token, rest: List[Token]): (PrefixExpression, List[Token]) = {
    val (right, restAfterParseExpression) = parseExpression(rest)
    (PrefixExpression(prefixToken.Literal, right), restAfterParseExpression)
  }

  def parseOperationExpression(integer: INT, operator: Token, rest: List[Token]): (Expression, List[Token]) = {
    val (right, restAfterParseExpression) = parseExpression(rest)
    (Operation(parseIntegerLiteral(integer), operator.Literal, right), restAfterParseExpression)
  }

  def parseExpression(tokens: List[Token]): (Expression, List[Token]) = tokens match {
    case (curr: IDENT) :: rest => (parseIdentifier(curr), rest)
    case (curr: INT) :: SEMICOLON :: rest  => (parseIntegerLiteral(curr), rest)
    case (curr @ (BANG | MINUS)) :: rest => parsePrefixExpression(curr)
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
}


/**
  Prefix expressions:
    BANG
    MINUS
  Identifier:
    IDENT
  IntegerLiteral:
    INT


  */