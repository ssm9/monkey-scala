package parsing

import lexing.Token

sealed trait Node

object Node {

  trait Statement extends Node

  trait Expression extends Node

  case class Identifier(token: Token) extends Expression

  case class LetStatement(token: Token, name: Identifier, value: Expression)
    extends Statement

  case class Program(statements: List[Statement]) extends Node

}