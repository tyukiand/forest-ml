package forestml.refimpl

/** Token type, specific to concrete syntax tree of Forest-ML */
sealed trait ForestMlTokenType
case object LeftParen extends ForestMlTokenType
case object RightParen extends ForestMlTokenType
case object LeftVerbDelim extends ForestMlTokenType
case object RightVerbDelim extends ForestMlTokenType
case object LeftIndentDelim extends ForestMlTokenType
case object RightIndentDelim extends ForestMlTokenType

case object DoubleQuote extends ForestMlTokenType

/** Start of a tree node.
  * 
  * A single `/`-character standing there on its own and 
  * designating the start of the tree node. 
  * The `/`-character is used in many other ways, but they all
  * are eliminated during the lexing stage.
  */
case object TreeStart extends ForestMlTokenType 

/** `%%`, escapes percent sign. */
case object EscapeEscape extends ForestMlTokenType

/** `%/`, escapes a single solidus. */
case object EscapeDisruptor extends ForestMlTokenType

/** `%"`, escapes a double quote within quoted text */
case object EscapeDoubleQuote extends ForestMlTokenType

/** `%n`, escapes newline character `U+000A` */
case object EscapeNewline extends ForestMlTokenType

/** `%r`, escapes carriage return. */
case object EscapeCarriageReturn extends ForestMlTokenType

/** `%b`, escapes the Backspace control character. */
case object EscapeBackspace extends ForestMlTokenType

/** `%t`, escapes tab */
case object EscapeTab extends ForestMlTokenType

/** The `%(` escape that appears in quoted text. */
case object EscapeUnicodeDecLeft extends ForestMlTokenType

/** The `%u(` escape taht appears in quoted text. */
case object EscapeUnicodeHexLeft extends ForestMlTokenType

/** Standalone `+`. */
case object Plus extends ForestMlTokenType


case object CharsDecimal extends ForestMlTokenType

/** `0-9`, `a-f`, `A-F`. */
case object CharsHexadecimal extends ForestMlTokenType
case object CharsIdentifier extends ForestMlTokenType
case object CharsGeneral extends ForestMlTokenType

sealed trait GenWhitespace extends ForestMlTokenType
case object Whitespace extends GenWhitespace
case object LeftCommentDelim extends GenWhitespace
case object RightCommentDelim extends GenWhitespace
case object CommentBody extends GenWhitespace
case object LineBreak extends GenWhitespace
case object IndentationBarrier extends GenWhitespace

object ForestMlTokenType {
  type Gws = List[Token[GenWhitespace]]
}