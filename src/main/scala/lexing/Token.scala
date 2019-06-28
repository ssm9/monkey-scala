package lexing

sealed trait Token {
  def Type: String
  def Literal: String

  override def toString: String = s"""{$Type : "$Literal"}"""
}

object Token {

  case class ILLEGAL(illegalLiteral: String) extends Token {
    def Type = "ILLEGAL"
    def Literal: String = illegalLiteral
  }

  case object EOF extends Token {
    def Type = "EOF"
    def Literal = "EOF"
  }

  // identifiers & int literals
  case class IDENT(identLiteral: String) extends Token {
    def Type = "IDENT"
    def Literal: String = identLiteral
  }

  case class INT(intLiteral: String) extends Token {
    def Type = "INT"
    def Literal: String = intLiteral
  }

  // operators
  case object ASSIGN extends Token {
    def Type = "ASSIGN"
    def Literal = "="
  }

  case object PLUS extends Token {
    def Type = "PLUS"
    def Literal = "+"
  }

  case object MINUS extends Token {
    def Type = "MINUS"
    def Literal = "-"
  }

  case object BANG extends Token {
    def Type = "BANG"
    def Literal = "!"
  }

  case object ASTERISK extends Token {
    def Type = "ASTERISK"
    def Literal = "*"
  }

  case object SLASH extends Token {
    def Type = "SLASH"
    def Literal = "/"
  }

  case object LT extends Token {
    def Type = "LT"
    def Literal = "<"
  }

  case object LTE extends Token {
    def Type = "LTE"
    def Literal = "<="
  }

  case object GT extends Token {
    def Type = "GT"
    def Literal = ">"
  }

  case object GTE extends Token {
    def Type = "GTE"
    def Literal = ">="
  }

  case object EQ extends Token {
    def Type = "EQ"
    def Literal = "=="
  }

  case object NOT_EQ extends Token {
    def Type = "NOT_EQ"
    def Literal = "!="
  }

  // delimiters
  case object COMMA extends Token {
    def Type = "COMMA"
    def Literal = ","
  }

  case object SEMICOLON extends Token {
    def Type = "SEMICOLON"
    def Literal = ";"
  }

  case object LPAREN extends Token {
    def Type = "LPAREN"
    def Literal = "("
  }

  case object RPAREN extends Token {
    def Type = "RPAREN"
    def Literal = ")"
  }

  case object LBRACE extends Token {
    def Type = "LBRACE"
    def Literal = "{"
  }

  case object RBRACE extends Token {
    def Type = "RBRACE"
    def Literal = "}"
  }

  case object LBRACK extends Token {
    def Type = "LBRACK"
    def Literal = "["
  }

  case object RBRACK extends Token {
    def Type = "RBRACK"
    def Literal = "]"
  }

  // keywords
  case object FUNCTION extends Token {
    def Type = "FUNCTION"
    def Literal = "FUNCTION"
  }

  case object LET extends Token {
    def Type = "LET"
    def Literal = "LET"
  }

  case object TRUE extends Token {
    def Type = "TRUE"
    def Literal = "TRUE"
  }

  case object FALSE extends Token {
    def Type = "FALSE"
    def Literal = "FALSE"
  }

  case object IF extends Token {
    def Type = "IF"
    def Literal = "IF"
  }

  case object ELSE extends Token {
    def Type = "ELSE"
    def Literal = "ELSE"
  }

  case object RETURN extends Token {
    def Type = "RETURN"
    def Literal = "RETURN"
  }

}