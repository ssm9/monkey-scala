package parsing

import lexing._
import lexing.Token._
import parsing.Node._

object Parser {

  private def parseExpressions(tokens: List[Token]): (Expression, List[Token]) = tokens match {
    case (curr: INT) :: rest  => (Identifier(curr.Literal), rest)
  }

  def parseIntegerLiteral(integer: INT): Expression = IntegerLiteral(Integer.parseInt(integer.Literal))

  def parseOperationExpression(integer: INT, operator: Token, rest: List[Token]): (Node, List[Token]) = {
    val (right, restAfterParseExpression) = parseExpressions(rest)
    (Operation(parseIntegerLiteral(integer), operator, right), restAfterParseExpression)
  }

  def parseGroupedExpression(): (Node, List[Token]) = ???

  def parseLetStatement(tokens: List[Token]): (Node, List[Token]) = tokens match {
    case (name: IDENT) :: ASSIGN :: (curr: INT) :: SEMICOLON :: rest => (LetStatement(Identifier(name.Literal), parseIntegerLiteral(curr)), rest)
    case (curr: INT) :: (operator @ (PLUS|MINUS)) :: rest => parseOperationExpression(curr, operator, rest)
    case LPAREN :: rest => parseGroupedExpression()
  }

  def parseStatement(tokens: List[Token]): (Node, List[Token]) = tokens match {
    case LET :: rest => parseLetStatement(rest)
    case EOF :: _ => (Eof, List[Token]())
  }
  def parse(tokens: List[Token]): List[Node] = {
    def getNodes(nodes: List[Node], tokens: List[Token]): List[Node] = parseStatement(tokens) match {
      case (Eof, _) => (Eof +: nodes).reverse
      case (node, rest) => getNodes(node +: nodes, rest)
    }

    getNodes(List[Node](), tokens)
  }
}