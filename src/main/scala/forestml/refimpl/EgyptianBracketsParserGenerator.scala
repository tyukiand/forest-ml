package forestml.refimpl

import scala.language.higherKinds

import EgyptianBrackets.Egypticity

object EgyptianBracketsParserGenerator 
extends ParserGenerator[EgyptianBracketsHints] {

  import EgyptianBrackets._

  trait ==>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  object ==> {
    def memoize[F[_], G[_]](f: F ==> G): F ==> G = {
      val hm = collection.mutable.HashMap.empty[F[_], G[_]]
      new (F ==> G) {
        def apply[A](x: F[A]): G[A] =
          hm.getOrElseUpdate(x, f(x)).asInstanceOf[G[A]]
      }
    }
  }

  def generateParser[TokTyp, TokWs <: TokTyp, A](
    g: Grammar[TokTyp, List[Token[TokWs]]],
    hints: EgyptianBracketsHints[TokTyp]
  )(lang: g.L[A]): Parser[TokTyp, TokWs, A] = {
    new Parser[TokTyp, TokWs, A] with ParserCombinators {

      type TT = TokTyp
      type W = TokWs
      type H = EgyptianBrackets
      import g._

      lazy val rec: L ==> PC = new (L ==> PC) {
        def apply[A](lang: L[A]): PC[A] = lang match {
          case r: Rule[A @unchecked] => rules(r) // look it up, memoized
          case s: SingleToken => eat(s.tokenType)
          case Empty => unit.asInstanceOf[PC[A]] /* A = Unit */
          case Concat(a, b) => {
            rec(a).syncOnSet(g.firstSet(b)) zip rec(b)
          }
          case PairedDelimiters(openingType, body, closingType) => {
            val opening = rec(SingleToken(openingType))
            val synchedBody = rec(body).syncOnSet(Set(closingType))
            val closing = rec(SingleToken(closingType))
            
            opening.flatMapInfallible {
              case (openingWhitespace, opTok) => {
                val closingWithWarning = closing.warnIf((ws_clTok, ctx) => {
                  val (_, clTok) = ws_clTok
                  val egyPars = ctx.errorRecoveryHeuristic
                  !egyPars.areMatchingBrackets(
                    opTok.tokenIdx,
                    clTok.tokenIdx
                  )
                })((ws_clTok, ctx) => {
                  val (_, clTok) = ws_clTok
                  val egyPars = ctx.errorRecoveryHeuristic
                  new ParseWarning(
                    clTok.tokenIdx,
                    "WARN-MATCHING-PAREN-BAD-INDENT",
                    "Indentation or linewise nesting level mismatch between " +
                    "opening paren in line " +
                    (1 + egyPars.tokenToLine(opTok.tokenIdx)) +
                    "and closing paren in line " +
                    (1 + egyPars.tokenToLine(clTok.tokenIdx)) +
                    "."
                  )
                })
                synchedBody.zip(closingWithWarning).map {
                  // third component carries no information, must be closing
                  // paren token.
                  case (a, (closingWhitespace, _)) =>
                    (openingWhitespace, a, closingWhitespace)
                }
              }
            }
          }
          case Choice(langs) =>
            branch(for { l <- langs } yield (firstSet(l), rec(l)))
          case Opt(a) => onlyIfStartsWith(firstSet(a), rec(a))
          case Rep(a) => repeatWhileStartsWith(firstSet(a), rec(a))
          case Mapped(l, f) => rec(l) map f
          case e: Ensuring[A @unchecked] =>
            rec(e.lang).ensuring(e.predicate, e.errMsg)
          case EndOfInput => eof.asInstanceOf[PC[A]]
        }
      }

      // `==>` is the companion object, it has helper method `memoize`.
      lazy val rules: Rule ==> PC = ==>.memoize ( new (Rule ==> PC) {
        def apply[A](lang: Rule[A]): PC[A] = lang.instantiated match {
          case r: Rule[_] => {
            throw new Error(s"Fatal error: rule wrapped in a rule: ${r.name}")
          }
          case sthElse => delay { rec(sthElse) }
        }
      })

      lazy val entryPoint: PC[A] = rec(lang)

      def parse(tokens: List[Token[TT]]): (List[ParseError], Option[A]) = {
        val input = ParserInput.fromTokens[TT, W](tokens, g.isWhitespace)
        val errAcc = AccumulatorBuilder.empty[ParseError]
        val egyptianBrackets = EgyptianBrackets(tokens, hints)
        val ctx0 = SynchronizationContext.empty(egyptianBrackets)
        val (_, e1, parseRes) = entryPoint(input, errAcc, ctx0)
        val oA = parseRes match {
          case ParseSuccess(a) => Some(a)
          case ParseFailure => None
          case Resynchronize(_) => throw new AssertionError(
            "Impossible resynchronization request at top level."
          )
        }
        (e1.result, oA)
      }

    }
  }
}
