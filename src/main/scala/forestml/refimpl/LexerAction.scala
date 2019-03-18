package forestml.refimpl

import LexerInput.{Eof => EofChar}

trait LexerAction[TT, +A] { outer =>
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] ( 
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (A, LexerInput, AccTok, AccErr)

  def flatMap[B](f: A => LexerAction[TT, B]): LexerAction[TT, B] = {
    new LexerAction[TT, B] {
      def apply[
        AccTok <: Accumulator[Token[TT], AccTok],
        AccErr <: Accumulator[SyntaxError, AccErr]
      ] ( 
        input: LexerInput,
        tokenAccumulator: AccTok,
        errorAccumulator: AccErr
      ): (B, LexerInput, AccTok, AccErr) = {
        val (a, i, t, e) = outer(input, tokenAccumulator, errorAccumulator)
        val tab = f(a)
        tab(i, t, e)
      }
    }
  }

  def map[B](f: A => B): LexerAction[TT, B] = 
    flatMap(a => LexerAction.pure(f(a)))

  def andThen[B](t: LexerAction[TT, B]): LexerAction[TT, B] = flatMap(_ => t)
}

object LexerAction {
  def pure[TT, A](a: A): LexerAction[TT, A] = new LexerAction[TT, A] {
    def apply[
      AccTok <: Accumulator[Token[TT], AccTok],
      AccErr <: Accumulator[SyntaxError, AccErr]
    ](
      input: LexerInput,
      tokenAccumulator: AccTok,
      errorAccumulator: AccErr
    ): (A, LexerInput, AccTok, AccErr) =
      (a, input, tokenAccumulator, errorAccumulator)
  }

  def unit[TT]: LexerAction[TT, Unit] = pure( () )
}

/* 
 * Implementation hint: it's needed only for a very restricted
 * class of strings, mostly strings that looks like ends of comments;
 * This can be made "string-less", we need only very special
 * strings of shape like e.g. `!++++/`, which can be represented by few
 * chars and an integer.
 */
case class SeekNextOccurrence[TT](substring: String)
extends LexerAction[TT, Boolean] {
  private val n = substring.size
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (Boolean, LexerInput, AccTok, AccErr) = {
    var offset = 0
    // $COVERAGE-OFF$
    while (true) {
      // $COVERAGE-ON$
      var couldMatch = true
      var idx = 0
      while (idx < n && couldMatch) {
        input.peek(offset + idx) match {
          case EofChar => {
            return (false, input, tokenAccumulator, errorAccumulator)
          }
          case c => {
            if (c == substring(idx)) {
              if (idx == n - 1) {
                return (
                  true,
                  input.advance(offset),
                  tokenAccumulator,
                  errorAccumulator
                )
              } else {
                /* must check more chars, continue */
              }
            } else {
              couldMatch = false
            }
          }
        }
        idx += 1
      }
      offset += 1
      // $COVERAGE-OFF$
    }
    throw new Error("Unreachable ID-23456787776545432345")
    // $COVERAGE-ON$
  }
}


case class Emit[TT](typ: TT) extends LexerAction[TT, Unit] {
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (Unit, LexerInput, AccTok, AccErr) = {
    val (newLexerInput, tok) = input.emit(typ)
    val newAcc = tokenAccumulator.append(tok)
    ((), newLexerInput, newAcc, errorAccumulator)
  }
}


/** Skip everything from `lexemeStart` to `forward`,
  * so that `forward` becomes new `lexemeStart`.
  */
case class Skip[TT]() extends LexerAction[TT, Unit] {
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (Unit, LexerInput, AccTok, AccErr) = {
    val newLexerInput = input.skip
    ((), newLexerInput, tokenAccumulator, errorAccumulator)
  }
}


case class Err[TT](id: String, msg: String) extends LexerAction[TT, Unit] {
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (Unit, LexerInput, AccTok, AccErr) = {
    val e = input.error(id, msg)
    val newError = errorAccumulator.append(e)
    ((), input, tokenAccumulator, newError)
  }
}

case class Advance[TT](dForward: Int) extends LexerAction[TT, Unit] {
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (Unit, LexerInput, AccTok, AccErr) = {
    val newLexerInput = input.advance(dForward)
    ((), newLexerInput, tokenAccumulator, errorAccumulator)
  }
}

sealed trait PeekSwitchMatcher

case object Default extends PeekSwitchMatcher
case object Eof extends PeekSwitchMatcher
trait CharacterMatcher extends PeekSwitchMatcher {
  def matches(c: Char): Boolean
}

case class ConstantCharMatcher(c: Char) extends CharacterMatcher {
  def matches(x: Char) = c == x
}

case class CharPredicateMatcher(p: Char => Boolean) extends CharacterMatcher {
  def matches(x: Char) = p(x)
}


case class PeekSwitch[TT, A](
  cases: List[(PeekSwitchMatcher, LexerAction[TT, A])]
)
extends LexerAction[TT, A] {

  private val eofHandler = cases
    .find(_._1 == Eof)
    .map(_._2)
    .getOrElse{
      // $COVERAGE-OFF$
      throw new Error("No Eof handler")
      // $COVERAGE-ON$
    }

  private val defaultHandler = cases
    .find(_._1 == Default)
    .map(_._2)
    .getOrElse{
      // $COVERAGE-OFF$
      throw new Error("No Default handler")
      // $COVERAGE-ON$
    }

  private val characterHandlers: List[(CharacterMatcher, LexerAction[TT, A])] =
    cases.collect{ case (c: CharacterMatcher, t) => (c, t) }

  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (A, LexerInput, AccTok, AccErr) = {
    input.peek(0) match {
      case EofChar => eofHandler(input, tokenAccumulator, errorAccumulator)
      case c => {
        for ((lhs, action) <- characterHandlers) {
          if (lhs.matches(c)) {
            return action(input, tokenAccumulator, errorAccumulator)
          }
        }
        defaultHandler(input, tokenAccumulator, errorAccumulator)
      }
    }
  }
}

case class Rep[TT](predicate: Char => Boolean) extends LexerAction[TT, Int] {
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (Int, LexerInput, AccTok, AccErr) = {
    var notDone = true
    var result = 0
    while(notDone) {
      val c = input.peek(result)
      c match {
        case EofChar => notDone = false
        case k => {
          if (predicate(k)) {
            result += 1
          } else {
            notDone = false
          }
        }
      }
    }
    (result, input.advance(result), tokenAccumulator, errorAccumulator)
  }
}

case class JoinRep1[TT](charClasses: List[(CharacterMatcher, TT)])
extends LexerAction[TT, Unit] {
  def apply[
    AccTok <: Accumulator[Token[TT], AccTok],
    AccErr <: Accumulator[SyntaxError, AccErr]
  ] (
    input: LexerInput,
    tokenAccumulator: AccTok,
    errorAccumulator: AccErr
  ): (Unit, LexerInput, AccTok, AccErr) = {
    var remainingClasses = charClasses
    var i = input
    var smallestClassUntilNow: Option[TT] = None
    // $COVERAGE-OFF$
    while(true) {
      // $COVERAGE-ON$
      val c = i.peek(0)
      c match {
        case EofChar => {
          // will (and must) crash if input is empty,
          // there is no meaningful alternative
          // $COVERAGE-OFF$
          assert(
            smallestClassUntilNow.isDefined,
            "Usage error: JoinRep1 called where no token can start; " +
            "Make sure that `joinRep1` is never invoked unless it can scan " +
            "at least one character."
          )
          // $COVERAGE-ON$
          val (newLexerInput, tok) = i.emit(smallestClassUntilNow.get)
          val newToks = tokenAccumulator.append(tok)
          return ((), newLexerInput, newToks, errorAccumulator)
        }
        case k => {
          var minClassUndetermined = true
          var fitsIntoPreviousClass = smallestClassUntilNow.isDefined
          while (minClassUndetermined) {
            val (hPred, hTyp) = remainingClasses.head
            if (hPred.matches(k)) {
              // head points to a class to which all previous chars belong
              minClassUndetermined = false
              if (!fitsIntoPreviousClass) {
                smallestClassUntilNow = Some(hTyp)
              }
            } else {
              // current character does not satisfy `hPred`
              fitsIntoPreviousClass = false
              remainingClasses = remainingClasses.tail
              if (remainingClasses.isEmpty) {
                minClassUndetermined = false
              }
            }
          }

          if (remainingClasses.isEmpty) {
            val (newLexerInput, tok) = i.emit(smallestClassUntilNow.get)
            val newToks = tokenAccumulator.append(tok)
            return ((), newLexerInput, newToks, errorAccumulator)
          }
        }
      }
      i = i.advance(1)
      // $COVERAGE-OFF$
    }
    throw new Error("Unreachable; ID-852735284691")
    // $COVERAGE-ON$
  }
}


/** Little DSL for tokenizer action construction */
trait LexerActions {
  type TT

  def is(c: Char) = ConstantCharMatcher(c)
  def pred(p: Char => Boolean) = CharPredicateMatcher(p)
  
  def seekNext(substring: String) = SeekNextOccurrence[TT](substring)
  def emit(t: TT) = Emit(t)
  def skip = Skip[TT]()
  def err(id: String, msg: String) = Err[TT](id, msg)
  def advance(i: Int) = Advance[TT](i)
  def peekSwitch[A](cases: (PeekSwitchMatcher, LexerAction[TT, A])*) =
    PeekSwitch(cases.toList)
  def rep(pred: Char => Boolean) = Rep[TT](pred)
  def cannotHappen[TT](msg: String)
  : LexerAction[TT, Nothing] = new LexerAction[TT, Nothing] {
    def apply[
      AccTok <: Accumulator[Token[TT], AccTok],
      AccErr <: Accumulator[SyntaxError, AccErr]
    ] (
      input: LexerInput,
      tokenAccumulator: AccTok,
      errorAccumulator: AccErr
    ): (Nothing, LexerInput, AccTok, AccErr) = throw new Error(msg)
  }
  
  def joinRep1(nestedClasses: (CharacterMatcher, TT)*) =
    JoinRep1(nestedClasses.toList)
  def pure[A](a: A): LexerAction[TT, A] = LexerAction.pure[TT, A](a)
  def unit: LexerAction[TT, Unit] = LexerAction.unit[TT]
}
