package parsing

import lexing._
import lexing.Token._
import parsing.Node._

object Parser extends ParseFunctions {

  def parseStatement(tokens: List[Token]): (Statement, List[Token]) = tokens match {
    case LET :: rest => parseLetStatement(rest)

    case RETURN :: rest => parseReturnStatement(rest)

    case default => parseExpressionStatement(default)

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