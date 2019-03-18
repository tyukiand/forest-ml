package forestml.refimpl

object ForestMlBracketHints extends EgyptianBracketsHints[ForestMlTokenType] {
  def isIndentationWhitespace(typ: ForestMlTokenType): Boolean = 
    typ.isInstanceOf[GenWhitespace] && typ != LineBreak
  def isLineBreak(typ: ForestMlTokenType): Boolean = typ == LineBreak
  def isOpening(typ: ForestMlTokenType): Boolean = {
    typ == LeftParen ||
    typ == EscapeUnicodeDecLeft ||
    typ == EscapeUnicodeHexLeft
  }
  def isClosing(typ: ForestMlTokenType): Boolean = typ == RightParen
}
