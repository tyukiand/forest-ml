package forestml.refimpl

trait Parser[TT, W <: TT, +A] {
  def parse(tokens: List[Token[TT]]): (List[ParseError], Option[A])
}