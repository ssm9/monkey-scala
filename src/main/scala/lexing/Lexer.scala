package lexing

import Token._

object Lexer {
  /**
    * Given a raw string from the source code, figure out if it's a keyword or an identifier
    * @param ident - the string of the potential identifier
    * @return a Token of the ident
    */
  private def identMatch(ident: String): Token =  ident match {
      case "fn"     => FUNCTION
      case "let"    => LET
      case "true"   => TRUE
      case "false"  => FALSE
      case "if"     => IF
      case "else"   => ELSE
      case "return" => RETURN
      case i        => IDENT(i)
  }

  private def getIdentifier(soFar: String, remaining: List[Char]): (Token, List[Char]) = remaining match {
      case curr :: rest if curr.isLetter || curr.isDigit => getIdentifier(soFar :+ curr, rest)
      case ls => (identMatch(soFar), ls)
  }

  private def getInteger(soFar: String, remaining: List[Char]): (Token, List[Char]) = remaining match {
      case curr :: rest if curr.isDigit || curr == '.' => getInteger(soFar :+ curr, rest)
      case ls => (INT(soFar), ls)
  }

  /**
    * Given a list of characters representing the code, get the next token out of them
    * @param chars - the list of chars constructed from the code
    * @return a Tuple2 of the next lexing.Token and a truncated list of chars, with the chars making up the token taken out
    */
  private def getToken(chars: List[Char]): (Token, List[Char])= chars match {
      /*
      this is the only recursion in this function, which is ok cause it only gets rid of the whitespace
       */
      case curr :: rest if curr.isWhitespace => getToken(rest)

      case curr :: rest if curr.isDigit => getInteger(curr.toString, rest)

      case curr :: rest if curr.isLetter => getIdentifier(curr.toString, rest)

      case '=' :: '=' :: rest => (EQ, rest)
      case '=' :: rest =>        (ASSIGN, rest)
      case '+' :: rest =>        (PLUS, rest)
      case '-' :: rest =>        (MINUS, rest)
      case '!' :: '=' :: rest => (NOT_EQ, rest)
      case '!' :: rest =>        (BANG, rest)
      case '/' :: rest =>        (SLASH, rest)
      case '*' :: rest =>        (ASTERISK, rest)
      case '<' :: rest =>        (LT, rest)
      case '>' :: rest =>        (GT, rest)
      case ';' :: rest =>        (SEMICOLON, rest)
      case ',' :: rest =>        (COMMA, rest)
      case '(' :: rest =>        (LPAREN, rest)
      case ')' :: rest =>        (RPAREN, rest)
      case '{' :: rest =>        (LBRACE, rest)
      case '}' :: rest =>        (RBRACE, rest)
      case '[' :: rest =>        (LBRACK, rest)
      case ']' :: rest =>        (RBRACK, rest)

      case curr :: rest => (ILLEGAL(curr.toString), rest)

      case Nil => (EOF, List[Char]())
  }

  def tokenize(chars: List[Char]): List[Token] = {
    def getTokens(tokens: List[Token], chars: List[Char]): List[Token] = getToken(chars) match {
      case (EOF, _) => (EOF +: tokens).reverse
      case (token, rest) => getTokens(token +: tokens, rest)
    }

    getTokens(List[Token](), chars)
  }
}
