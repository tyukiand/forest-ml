package forestml.refimpl

import cats.Semigroupal
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.syntax.validated._

/* TODO: temporarily disabled to make it compile again.
object ForestMlEvaluation {
  sealed trait Type
  case class Text extends Type
  case class Tree extends Type
  case class Forest extends Type

  import ForestMlAst._

  type TypeErrorOr[+T] = ValidatedNec[TypeError, T]

  def check(term: Juxtaposition): TypeErrorOr[Type] = {
    ???
  }

  def check(term: MarkupSum): TypeErrorOr[Type] = { ???
    // val MarkupSum(summands) = term
    // summands.map(check)
  }

  def check(markup: Markup): TypeErrorOr[Type] = {
    // val Markup(fragments) = markup
    // val fragmentTypes = 
    ???
  }

  def check(fragment: MarkupFragment): TypeErrorOr[Type] = fragment match {
    case UnicodeHexEscape(digits) => {
      if (digits.size > 8) {
        NonEmptyChain.one(new TypeError(
          digits,
          "FORML-TYP-ERR-HEX-UNICODE-TOO-LONG",
          s"Only 1-8 hexadecimal digits allowed in unicode escape sequences, " +
          s"${digits.getString} is too long."
        )).invalid[Type]
      } else {
        val longCodePoint = Long.parseInt(d.getString, 16)
        if (longCodePoint > 0x10FFFFL) {
          NonEmptyChain.one(new TypeError(
            digits,
            "FORML-TYP-ERR-HEX-UNICODE-TOO-LARGE",
            "Only code points in the range 0 - 0010FFFF acceptable, " + 
            s" but got: ${digits.getString}."
          )).invalid[Type]
        } else {
          val codePoint = longCodePoint.toInt
        }
      }
    }
  }
}
*/
