package forestml.refimpl

abstract class SyntaxError(id: String, message: String) {
  def charPosition: Int
}

class LexicalAnalysisError(offset: Int, id: String, message: String)
extends SyntaxError(id, message) {
  def charPosition = ??? // TODO
  override def toString = s"Lexical error at $offset [$id]: $message"
}

/** Contains index of the problematic token and an error message. */
class ParseError(tokenIndex: Int, id: String, message: String)
extends SyntaxError(id, message) {
  def charPosition = ??? // TODO
  override def toString = s"Parse error at $tokenIndex [$id]: $message"
}

class ParseWarning(tokenIndex: Int, id: String, msg: String)
extends ParseError(tokenIndex, id, msg) {
  override def toString = s"Warning at $tokenIndex [$id]: $msg"
}

class TypeError(lexeme: StringSlice, id: String, message: String)
extends SyntaxError(id, message) {
  def charPosition = ???
  override def toString = s"Type error at ${lexeme.start}: $message"
}
