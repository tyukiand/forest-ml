package forestml.refimpl

/** A token consisting of the actual lexeme (stringSlice) and a token type. */
case class Token[+TT](stringSlice: StringSlice, tokenType: TT, tokenIdx: Int) {
  // $COVERAGE-OFF$
  override def toString = s"Tok($tokenType, `${stringSlice}`)"
  // $COVERAGE-ON$
}
