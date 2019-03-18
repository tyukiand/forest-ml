package forestml.refimpl

/** Interface exposed to the consumer of the input */
trait LexerInput {
  def peek(offset: Int): Char
  def advance(i: Int): LexerInput
  def error(id: String, message: String): LexicalAnalysisError
  def emit[TT](tokenType: TT): (LexerInput, Token[TT])
  def skip: LexerInput

  /** Helper method for whitebox-tests */
  private[refimpl] def position: Int

  def isEof = peek(0) == LexerInput.Eof
}


case class StringLexerInput(
  underlying: String,
  lexemeStart: Int,
  forwardPointer: Int,
  currentTokenIdx: Int = 0
) extends LexerInput {
  private[refimpl] def position = lexemeStart
  
  private val length = underlying.size

  def peek(offset: Int): Char = {
    val idx = forwardPointer + offset
    if (idx < length) underlying(idx)
    else '\uFFFF'
  }

  def advance(i: Int) = {
    // $COVERAGE-OFF$
    require(i >= 0, "The `advance` must be non-negative")
    // $COVERAGE-ON$
    StringLexerInput(underlying, lexemeStart, forwardPointer + i)
  }

  def error(id: String, msg: String): LexicalAnalysisError = {
    new LexicalAnalysisError(forwardPointer, id, msg)
  }

  def emit[TT](tokenType: TT): (StringLexerInput, Token[TT]) = {
    // $COVERAGE-OFF$
    require(
      forwardPointer > lexemeStart,
      s"Tokens must not be empty, but lexemeStart = $lexemeStart " +
      s"is the same as forwardPointer = $forwardPointer"
    )
    // $COVERAGE-ON$
    val i = StringLexerInput(
      underlying, 
      forwardPointer /* ! */,
      forwardPointer,
      currentTokenIdx + 1
    )
    val slice = StringSlice(underlying, lexemeStart, forwardPointer)
    val t = Token(slice, tokenType, currentTokenIdx)
    (i, t)
  }

  def skip: StringLexerInput = {
    // Almost same as `emit`, but without emitting anything
    StringLexerInput(
      underlying, 
      forwardPointer, 
      forwardPointer, 
      currentTokenIdx // don't advance token index
    )
  }

  override def isEof: Boolean = forwardPointer >= length

  // $COVERAGE-OFF$ 
  // Don't rely on any particular `toString` representation,
  // it's whatever seemed most useful for the last debugging session.
  override def toString: String = {
    val slice = underlying.slice(lexemeStart,
      (forwardPointer max (lexemeStart + 10)) min length
    )
    s"StringLexerInput(strt=$lexemeStart, fwd=$forwardPointer @ `$slice`)"
  }
  // $COVERAGE-ON$

}

object LexerInput {
  def fromString(s: String): LexerInput = StringLexerInput(s, 0, 0)
  val Eof: Char = '\uFFFF'
}
