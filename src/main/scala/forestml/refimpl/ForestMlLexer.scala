package forestml.refimpl

object ForestMlLexer extends Lexer with LexerActions {
  type TT = ForestMlTokenType
  type LA[X] = LexerAction[TT, X]


  lazy val token = peekSwitch(
    Eof -> cannotHappen(
      "`Token` rule invoked after Eof. Lexer broken!"
    ),
    is('"') -> {advance(1) andThen emit(DoubleQuote)},
    is('/') -> disrupted,
    is('(') -> {advance(1) andThen emit(LeftParen)},
    is(')') -> {advance(1) andThen emit(RightParen)},
    is('%') -> escape,
    is('+') -> {advance(1) andThen emit(Plus)},
    is('\r') -> newline,
    is('\n') -> newline,
    pred(_.isWhitespace) -> {advance(1) andThen whitespace},
    Default -> rawChars
  )

  lazy val disrupted = advance(1) andThen peekSwitch(
    Eof -> emit(TreeStart),
    is('/') -> {
      err(
        "FORML-ERR-LEX-SOLIDUS-2X", 
        "'/' followed by another '/' is invalid." +
        " Use `/\"\"/(...)` or `/(/(...))` if you wanted a " +
        " node with an empty string as name."
      ) andThen
      advance(1) andThen
      skip
    },
    is('*') -> commentTail,
    is('+') -> verbTail,
    is('!') -> verbTail,
    Default -> emit(TreeStart)
  )

  def aToF(c: Char): Boolean = (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  
  lazy val rawChars: LA[Unit] = joinRep1(
    pred(_.isDigit) -> CharsDecimal,
    pred(c => aToF(c) || c.isDigit) -> CharsHexadecimal,
    pred(c => 
      c.isLetterOrDigit || 
      c == '.' || 
      c == '_' || 
      c == ':' || 
      c == '-'
    ) -> CharsIdentifier,
    pred(c => !(
      c.isWhitespace ||
      c == '%' ||
      c == '"' ||
      c == '/' ||
      c == '(' ||
      c == ')'
    )) -> CharsGeneral
  )

  lazy val escape = advance(1) andThen peekSwitch(
    Eof -> {err(
      "FORML-ERR-LEX-ESC-EOF",
      "Unterminated escape sequence started with '%'"
    ) andThen skip},
    is('%') -> {advance(1) andThen emit(EscapeEscape)},
    is('/') -> {advance(1) andThen emit(EscapeDisruptor)},
    is('"') -> {advance(1) andThen emit(EscapeDoubleQuote)},
    is('n') -> {advance(1) andThen emit(EscapeNewline)},
    is('r') -> {advance(1) andThen emit(EscapeCarriageReturn)},
    is('b') -> {advance(1) andThen emit(EscapeBackspace)},
    is('t') -> {advance(1) andThen emit(EscapeTab)},
    is('(') -> {advance(1) andThen emit(EscapeUnicodeDecLeft)},
    is('u') -> {advance(1) andThen peekSwitch(
      is('(') -> {advance(1) andThen emit(EscapeUnicodeHexLeft)},
      Default -> {
        err(
          "FORML-ERR-LEX-UNICODE-MISSING-PAR",
          "Missing opening parenthesis after unicode escape. " +
          "The correct syntax is: `%u(HHHH)`, where `H` are hexadecimal " +
          "digits."
        ) andThen
        advance(1) andThen
        skip
      },
      Eof -> {err(
        "FORML-ERR-LEX-UNICODE-HIT-EOF",
        "Unicode escape sequence unexpectedly ended with end of file."
      ) andThen skip}
    )},
    Default -> { 
      err("FORML-ERR-LEX-INVALID-ESC", "Invalid escape sequence.") andThen 
      advance(1) andThen 
      skip
    }
  )

  lazy val whitespace: LA[Unit] = 
    rep(c => c.isWhitespace && c != '\n' && c != '\r') andThen emit(Whitespace)

  lazy val newline: LA[Unit] = peekSwitch(
    is('\r') -> {advance(1) andThen peekSwitch(
      is('\n') -> {advance(1) andThen emit(LineBreak) andThen maybeIndent},
      Eof -> emit(LineBreak),
      Default -> {emit(LineBreak) andThen maybeIndent}
    )},
    is('\n') -> {advance(1) andThen emit(LineBreak) andThen maybeIndent},
    Eof -> cannotHappen(
      "EOF is neither `\r` nor `\n`, cannot appear as a line-separator."
    ),
    Default -> cannotHappen(
      "Fatal error: `newline` called not at `\r` or `\n`. " +
      "This should never happen, ID-62987205674"
    )
  )

  lazy val maybeIndent = for {
    k <- rep(c => c.isWhitespace && c != '\n' && c != '\r') 
    _ <- if (k == 0) unit else peekSwitch (
      is('|') -> {advance(1) andThen emit(IndentationBarrier)},
      Default -> emit(Whitespace),
      Eof -> emit(Whitespace)
    )
  } yield ()

  lazy val commentTail = for {
    k <- rep(_ == '*')
    _ <- emit(LeftCommentDelim)
    validComment <- seekNext("*" * k + "/")
    _ <- (
      if (validComment) {
        emit(CommentBody) andThen
        advance(k + 1) andThen 
        emit(RightCommentDelim)
      } else {
        err("FORML-ERR-LEX-CMT-NOT-CLOSED", "Unclosed comment")
      }
    )
  } yield ()

  lazy val verbTail = rep(_ == '+').flatMap { k => 
    peekSwitch(
      Eof -> err(
        "FORML-ERR-LEX-VERB-INVALID-START-EOF",
        "Verbatim start escape sequence `/+++!` was not terminated by `!`, " +
        "hit end of input instead."
      ),
      is('!') -> {
        advance(1) andThen
        emit(LeftVerbDelim) andThen 
        seekNext("!" + ("+" * k) + "/").flatMap{ if(_) {
          emit(CharsGeneral) andThen 
          advance(k + 2) andThen 
          emit(RightVerbDelim)
        } else {
          err(
            "FORML-ERR-LEX-VERB-NOT-CLOSED",
            "Didn't find the closing `!" + "+" * k + "/` verbatim delimiter."
          )
        }}
      },
      Default -> err(
        "FORML-ERR-LEX-VERB-INVALID-START-OTHER",
        "Verbatim start escape sequence `/+++!` was not terminated by `!`"
      )
    )
  }

  /** Very first line is treated as if it were preceeded by a line break. */
  override lazy val specialStart = maybeIndent
}