package forestml.refimpl

/** (Immutable) iterator of a sequence of `Token`s, that additionally knows
  * how to emit positioned error messages at the current position.
  *
  * To instantiate a `ParserInput`, use the `fromTokens` method in the 
  * companion object.
  */
trait ParserInput[TT, W <: TT] {
  def peek(offset: Int): Option[Token[TT]]
  def peekType(offset: Int): Option[TT] = peek(offset).map(_.tokenType)
  def advance(k: Int): ParserInput[TT, W]

  def error(id: String, message: String): ParseError

  def stashedWhitespace: List[Token[W]]
  def skipWhitespace = advance(0)
}

object ParserInput {
  def fromTokens[TT, W <: TT](
    tokens: List[Token[TT]],
    isWhitespace: TT => Boolean
  ): ParserInput[TT, W] = ParserInputImpl[TT, W](tokens.toArray, isWhitespace)
}

class ParserInputImpl[TT, W <: TT](
  tokens: Array[Token[TT]],
  wsPos: Int,
  pos: Int,
  isWhitespace: TT => Boolean
) extends ParserInput[TT, W] {
  /* TODO: cover that by tests... 
  require(
    wsPos <= pos,
    "Fatal error, ParserInputImpl is broken: " +
    "skipped whitespace index larger than position."
  )
  */
  private val len = tokens.size
  def peek(offset: Int): Option[Token[TT]] = {
    val idx = pos + offset
    if (idx < len) {
      if (offset == 0 && isWhitespace(tokens(idx).tokenType)) {
        throw new AssertionError("ParserInput is broken, peeks whitespace at 0")
      }
      Some(tokens(idx))
    } else {
      None
    }
  }
  def advance(k: Int): ParserInput[TT, W] = {
    val newWsPos = pos + k
    var i = newWsPos
    while (i < len && isWhitespace(tokens(i).tokenType)) {
      i += 1
    }
    new ParserInputImpl(tokens, newWsPos, i, isWhitespace)
  }

  def error(id: String, msg: String): ParseError =
    new ParseError(pos, id, msg)

  def stashedWhitespace: List[Token[W]] = 
    (wsPos until pos).map{
      i => tokens(i).asInstanceOf[Token[W]]
    }(collection.breakOut)
}

object ParserInputImpl {
  def apply[TT, W <: TT](tokens: Array[Token[TT]], isWhitespace: TT => Boolean)
  : ParserInput[TT, W] = {
    val len = tokens.size
    var i = 0
    while (i < len && isWhitespace(tokens(i).tokenType)) {
      i += 1
    }
    new ParserInputImpl[TT, W](tokens, 0, i, isWhitespace)
  }
}
