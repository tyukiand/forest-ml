package forestml.refimpl

trait Lexer {

  type TT

  def token: LexerAction[TT, Unit]

  /** Called instead of `token` at the start.
    *
    * Should be overridden if the very first token has to 
    * be treaded slightly differently.
    */
  def specialStart: LexerAction[TT, Unit] = token

  def tokenize[ErrAcc <: Accumulator[SyntaxError, ErrAcc]](
    code: String,
    errAcc: ErrAcc
  ): (List[Token[TT]], ErrAcc) = {
    var tokAcc = AccumulatorBuilder.empty[Token[TT]]
    var input = LexerInput.fromString(code)
    var currErrAcc = errAcc
    var started = false
    var prevPos = -1
    while (!input.isEof) {
      val (_, i, t, e) =
        if (started) token(input, tokAcc, currErrAcc)
        else {
          started = true
          specialStart(input, tokAcc, currErrAcc)
        }
      input = i
      tokAcc = t
      currErrAcc = e
      var currPos = input.position
      if (currPos == prevPos) {
        // $COVERAGE-OFF$
        throw new Error(
          "Invalid lexical structure: lexer stuck at position " + currPos + 
          " (loops without progress), for input:\n" + input
        )
        // $COVERAGE-ON$
      } else {
        prevPos = currPos
      }
    }

    (tokAcc.result, currErrAcc)
  }

  /** Spherical `Lexer` in vacuum: tokenizes input code,
    * returns list of tokens and list of errors.
    * 
    * Does ''not'' return an error accumulator that would allow to insert
    * more errors.
    */ 
  def tokenize(code: String): (List[Token[TT]], List[SyntaxError]) = {
    val (toks, errAcc) = 
      tokenize(code, AccumulatorBuilder.empty[SyntaxError])
    (toks, errAcc.result)
  }
}