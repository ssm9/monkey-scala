package parsing

import lexing.Token

sealed trait Node

object Node {

  trait Statement extends Node

  trait Expression extends Node

  case class Identifier(value: String) extends Expression

  case class IntegerLiteral(value: Int) extends Expression

  case class StringLiteral(value: String) extends Expression

  case class Operation(left: Expression, operator: Token, right: Expression) extends Expression

  case class ExpressionStatement(value: Expression) extends Statement

  case class LetStatement(name: Identifier, value: Expression) extends Statement

  case class ReturnStatement(value: Expression) extends Statement

  case class Program(statements: List[Statement]) extends Node

  case object Eof extends Statement
}

/**
  Node.Token.Literal for getting the literal value of a Node
  */