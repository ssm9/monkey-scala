package lexing

sealed trait Token {
  val Type: String
  val Literal: String

  override def toString: String = s"""{$Type : "$Literal"}"""
}

object Token {

  case class ILLEGAL(illegalLiteral: String) extends Token {
    val Type = "ILLEGAL"
    val Literal: String = illegalLiteral
  }

  case object EOF extends Token {
    val Type = "EOF"
    val Literal = "EOF"
  }

  // identifiers & int literals
  case class IDENT(identLiteral: String) extends Token {
    val Type = "IDENT"
    val Literal: String = identLiteral
  }

  case class INT(intLiteral: String) extends Token {
    val Type = "INT"
    val Literal: String = intLiteral
  }

  // operators
  case object ASSIGN extends Token {
    val Type = "ASSIGN"
    val Literal = "="
  }

  case object PLUS extends Token {
    val Type = "PLUS"
    val Literal = "+"
  }

  case object MINUS extends Token {
    val Type = "MINUS"
    val Literal = "-"
  }

  case object BANG extends Token {
    val Type = "BANG"
    val Literal = "!"
  }

  case object ASTERISK extends Token {
    val Type = "ASTERISK"
    val Literal = "*"
  }

  case object SLASH extends Token {
    val Type = "SLASH"
    val Literal = "/"
  }

  case object LT extends Token {
    val Type = "LT"
    val Literal = "<"
  }

  case object LTE extends Token {
    val Type = "LTE"
    val Literal = "<="
  }

  case object GT extends Token {
    val Type = "GT"
    val Literal = ">"
  }

  case object GTE extends Token {
    val Type = "GTE"
    val Literal = ">="
  }

  case object EQ extends Token {
    val Type = "EQ"
    val Literal = "=="
  }

  case object NOT_EQ extends Token {
    val Type = "NOT_EQ"
    val Literal = "!="
  }

  // delimiters
  case object COMMA extends Token {
    val Type = "COMMA"
    val Literal = ","
  }

  case object SEMICOLON extends Token {
    val Type = "SEMICOLON"
    val Literal = ";"
  }

  case object LPAREN extends Token {
    val Type = "LPAREN"
    val Literal = "("
  }

  case object RPAREN extends Token {
    val Type = "RPAREN"
    val Literal = ")"
  }

  case object LBRACE extends Token {
    val Type = "LBRACE"
    val Literal = "{"
  }

  case object RBRACE extends Token {
    val Type = "RBRACE"
    val Literal = "}"
  }

  case object LBRACK extends Token {
    val Type = "LBRACK"
    val Literal = "["
  }

  case object RBRACK extends Token {
    val Type = "RBRACK"
    val Literal = "]"
  }

  // keywords
  case object FUNCTION extends Token {
    val Type = "FUNCTION"
    val Literal = "FUNCTION"
  }

  case object LET extends Token {
    val Type = "LET"
    val Literal = "LET"
  }

  case object TRUE extends Token {
    val Type = "TRUE"
    val Literal = "TRUE"
  }

  case object FALSE extends Token {
    val Type = "FALSE"
    val Literal = "FALSE"
  }

  case object IF extends Token {
    val Type = "IF"
    val Literal = "IF"
  }

  case object ELSE extends Token {
    val Type = "ELSE"
    val Literal = "ELSE"
  }

  case object RETURN extends Token {
    val Type = "RETURN"
    val Literal = "RETURN"
  }

}