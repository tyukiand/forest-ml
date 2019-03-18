package forestml.refimpl

import scala.language.higherKinds

/** Generates an in-memory `Parser` from a language of a `Grammar`.
  *
  * Not a "real" parser generator in the sense of emitting any code that
  * has to be compiled. It merely takes the abstract description in form
  * of a [[Grammar]] and builds a resynchronizing [[Parser]] using the
  * applicative parser combinators.
  */
trait ParserGenerator[H[_]] {

  /** Converts a grammar into a predictive parser.
    *
    * If the grammar is unsuitable for generating predictive parsers,
    * halts and catches fire (and that's OK, because it is tailored
    * to one single grammar which is not expected to change too much).
    *
    * `H` is 
    */
  def generateParser[TT, W <: TT, A](
    g: Grammar[TT, List[Token[W]]],
    heuristicHints: H[TT]
  )(lang: g.L[A]): Parser[TT, W, A]
}
