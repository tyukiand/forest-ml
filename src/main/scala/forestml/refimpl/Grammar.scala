package forestml.refimpl

import flow.FlowContext

trait Grammar[TT, W] {

  sealed trait L[+A] {
    def ~[B](other: L[B]): L[(A, B)] = Concat(this, other)
    def ~>[B](other: L[B]): L[B] = Concat(this, other) ^^ (_._2)
    def <~[B](other: L[B]): L[A] = Concat(this, other) ^^ (_._1)
    def |[B >: A](other: L[B]): L[B] = Choice[B](List(this, other))
    def ? : L[Option[A]] = Opt(this)
    def * : L[List[A]] = Rep(this)
    def + : L[List[A]] = this ~ this.* ^^ { case (a, as) => a :: as }
    def ^^[B](f: A => B): L[B] = Mapped(this, f)
    def ^^^[B](b: B): L[B] = Mapped(this, (_: A) => b)
    def ensuring(pred: A => Boolean, errMsg: String): L[A] = 
      Ensuring(this, pred, errMsg)
  }

  def isWhitespace(t: TT): Boolean

  case object Empty extends L[(W, Unit)]
  implicit class SingleToken(val tokenType: TT) extends L[(W, Token[TT])] {
    override def toString = tokenType.toString
  }

  /** Whitespace right before the end of file. */
  object EndOfInput extends L[W]

  /** A special concatenation that allows to specify a more sophisticated
    * resynchronization strategy for searching the "right" closing
    * parenthesis. Useful for paired parentheses, square brackets, curly
    * braces, etc.
    */
  case class PairedDelimiters[+A](opening: TT, content: L[A], closing: TT)
  extends L[(W, A, W)] {
    override def toString = s"{$opening $content $closing}"
  }

  case class Concat[+A, +B](a: L[A], b: L[B]) extends L[(A, B)] {
    override def toString = s"($a+$b)"
  }
  case class Choice[+A](langs: List[L[A]]) extends L[A] {
    override def |[B >: A](other: L[B]) = Choice[B](other :: langs)
  }
  case class Opt[+A](lang: L[A]) extends L[Option[A]]
  case class Rep[+A](lang: L[A]) extends L[List[A]]
  case class Mapped[A, +B](lang: L[A], f: A => B) extends L[B] {
    override def toString = lang.toString + "^"
  }

  class Rule[+A](val name: String, definition: () => L[A]) extends L[A] {
    lazy val instantiated: L[A] = definition()
    override def toString = name
  }
  
  case class Ensuring[A](
    lang: L[A],
    predicate: A => Boolean,
    errMsg: String
  ) extends L[A]

  def rule[A](name: String)(x: => L[A]): L[A] = new Rule(name, () => x)
  def inlined[A](x: => L[A]): L[A] = x

  private val flowContext = FlowContext.empty
  import flowContext.{delay => flowDelay, _}

  private def memoize[A, B](f: A => B): (A => B) = {
    val hm = collection.mutable.HashMap.empty[A, B]
    a => hm.getOrElseUpdate(a, f(a))
  }

  private lazy val isNullableFlows
  : L[_] => Flow[Boolean] = memoize[L[_], Flow[Boolean]] {
    case _: SingleToken => pure(false)
    case PairedDelimiters(_, _, _) => pure(false)
    case Concat(a, b) => accumulator(
      flowDelay { map2(isNullableFlows(a), isNullableFlows(b)){ _ && _ }},
      false
    )(_ || _)

    case Choice(langs) => accumulator(
      langs.map(l => flowDelay { isNullableFlows(l) }).toSet,
      false
    )(
      (oldBool: Boolean, changes: Set[Boolean]) => {
        oldBool || changes.exists(identity)
      }
    )

    case Opt(_) => pure(true)
    case Rep(_) => pure(true)
    case Mapped(l, _) => isNullableFlows(l)
    case Empty => pure(true)
    case d: Rule[_] => flowDelay { isNullableFlows(d.instantiated) }
    case e: Ensuring[_] => isNullableFlows(e.lang)
    case EndOfInput => pure(true)
  }

  def isNullable[A](lang: L[A]): Boolean = isNullableFlows(lang).get

  private lazy val firstSetFlows
  : (L[_] => Flow[Set[TT]]) = memoize[L[_], Flow[Set[TT]]] {
    case Empty => pure(Set.empty[TT])
    case s: SingleToken => pure(Set(s.tokenType))
    case Concat(a, b) => {
      if (isNullable(a)) {
        accumulator(
          flowDelay { map2(firstSetFlows(a), firstSetFlows(b))(_ ++ _) },
          Set.empty[TT]
        )(_ ++ _)
      } else {
        flowDelay { firstSetFlows(a) }
      }
    }
    case Choice(langs) => {
      accumulator(
        langs.map(l => flowDelay { firstSetFlows(l) }).toSet,
        Set.empty[TT]
      )( (oldSet, newSets) => newSets.foldLeft(oldSet)(_ ++ _) )
    }
    case Opt(a) => firstSetFlows(a)
    case Rep(a) => firstSetFlows(a)
    case Mapped(a, _) => firstSetFlows(a)
    case PairedDelimiters(a, b, c) => firstSetFlows(a)
    case d: Rule[_] => accumulator(
      flowDelay { firstSetFlows(d.instantiated) },
      Set.empty[TT]
    )(_ ++ _)
    case Ensuring(l, _, _) => firstSetFlows(l)
    case EndOfInput => pure(Set.empty)
  }

  def firstSet[A](lang: L[A]): Set[TT] = firstSetFlows(lang).get
}
