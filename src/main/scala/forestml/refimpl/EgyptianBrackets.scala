package forestml.refimpl

/** Additional hints that have to be provided together with the
  * grammar definition to the `ParserGenerator`.
  *
  * These additional hints do not define the language 
  * itself, but help to generate better error messages.
  */
trait EgyptianBracketsHints[TT] {
  def isIndentationWhitespace(typ: TT): Boolean
  def isLineBreak(typ: TT): Boolean
  def isOpening(typ: TT): Boolean
  def isClosing(typ: TT): Boolean
}

class EgyptianBrackets(
  indentationDescriptors: Array[Long],
  _tokenToLine: Array[Int],
  tokenToEgypticity: Array[EgyptianBrackets.Egypticity]
) {
  import EgyptianBrackets._

  /** Checks whether tokens at positions `aTokIdx` and
    * `bTokIdx` constitute a pair of matching "egyptian brackets".
    */
  def areMatchingBrackets(aTokIdx: Int, bTokIdx: Int): Boolean = {
    val aIndentDescr = indentationDescriptorForTokenIndex(aTokIdx)
    val bIndentDescr = indentationDescriptorForTokenIndex(bTokIdx)
    if (aIndentDescr != bIndentDescr) {
      // definitely not on same line,
      // lines don't even look similar -> No, don't match.
      false
    } else {
      val ae = egypticity(aTokIdx)
      val be = egypticity(bTokIdx)

      (ae, be) match {
        // TODO: heuristic can be improved for same-line tokens
        case (SameLineOpening, SameLineClosing) => true
        case (EgyptianOpening(x), EgyptianClosing(y)) => x == y
        case (_, _) => false
      }
    }
  }

  def indentationDescriptorForTokenIndex(tokIdx: Int): Long = {
    indentationDescriptors(_tokenToLine(tokIdx))
  }

  def egypticity(tokIdx: Int): Egypticity = tokenToEgypticity(tokIdx)

  def tokenToLine(tokIdx: Int): Int = _tokenToLine(tokIdx)

  // $COVERAGE-OFF$
  override def toString: String = {
    var prevLine = -1
    // side-effecty `for-yield` which relies on the variable!
    (for (i <- 0 until _tokenToLine.size) yield {
      val currLine = tokenToLine(i)
      val prefix = if (currLine != prevLine) {
        val nl = if (prevLine >= 0) "\n" else ""
        nl + WhitespaceHashing.decode(indentationDescriptors(currLine))
      } else {
        ""
      }
      prevLine = currLine
      prefix + egypticity(i)
    }).mkString
  }
  // $COVERAGE-ON$
}

object EgyptianBrackets {

  /** Executes action `doForLine` for each line and `doForSeparator` for
    * each line separator.
    */
  def foreachLine[A](
    list: List[A],
    isLineBreak: A => Boolean,
    doForLine: List[A] => Unit,
    doForSeparator: A => Unit
  ): Unit = {
    import collection.mutable.ListBuffer
    @annotation.tailrec
    def rec(rest: List[A]): Unit = rest match {
      case Nil => ()
      case _ => {
        val (line, nlNewRest) = rest.span(x => !isLineBreak(x))
        doForLine(line)
        if (nlNewRest.nonEmpty) {
          doForSeparator(nlNewRest.head)
          rec(nlNewRest.tail)
        }
      }
    }
    rec(list)
  }

  /** "Egypticity" is a rough description of a role of a token within a line.
    *
    * It is used by a resynchronization strategy for better error reporting
    * on code that is properly indented and uses some variant of the
    * "Egyptian brackets" style.
    *
    * Closing parentheses that have no matching opening parentheses on the same
    * line have [Closing] Egypticity. Opening parentheses that have no 
    * matching closing parentheses on the same line have [Opening] Egypticity.
    * All other tokens have [Neutral] Egypticity.
    *
    * Opening and closing brackets also store an `indentationDescriptor` - 
    * a hash that describes the whitespace at the beginning of the line.
    *
    * Egypticity type (opening / closing), index, and the indentation
    * descriptor allow to match up corresponding parentheses at different lines,
    * no matter how broken the input between those lines is, as long as it is
    * indented further than the enclosing parentheses.
    */
  sealed trait Egypticity
  case object Neutral extends Egypticity {
    // $COVERAGE-OFF$
    override def toString = "_"
    // $COVERAGE-ON$
  }
  case class EgyptianOpening(index: Int) extends Egypticity {
    // $COVERAGE-OFF$
    override def toString = s"+$index("
    // $COVERAGE-ON$
  }
  case class EgyptianClosing(index: Int) extends Egypticity {
    // $COVERAGE-OFF$
    override def toString = s")-$index"
    // $COVERAGE-ON$
  }
  case object SameLineOpening extends Egypticity {
    // $COVERAGE-OFF$
    override def toString = "<"
    // $COVERAGE-ON$
  }
  case object SameLineClosing extends Egypticity {
    // $COVERAGE-OFF$
    override def toString = ">"
    // $COVERAGE-ON$
  }

  private val EmptyLineHash = WhitespaceHashing.hash(StringSlice.whole(""))

  def lineEgypticity[TT](
    line: List[Token[TT]], 
    hints: EgyptianBracketsHints[TT]
  ): Seq[Egypticity] = {
    val IsSthElse: Byte = 0
    val IsOpening: Byte = 1
    val IsClosing: Byte = 2
    val initialClassification: Array[Byte] = line.map { t =>
      if (hints.isOpening(t.tokenType)) IsOpening
      else if (hints.isClosing(t.tokenType)) IsClosing
      else IsSthElse
    }(collection.breakOut)

    val n = initialClassification.size
    val egypticityTypeArray = Array.fill[Egypticity](n){ Neutral }
    var stack: List[(Int, Boolean)] = Nil // (idx, isClosing?)
    var numClosing = 0
    var numOpening = 0
    for (idx <- 0 until n) {
      val p = initialClassification(idx)
      if (p != IsSthElse) {
        stack = (p, stack) match {
          case (IsClosing, Nil) => {
            numClosing += 1
            List((idx, true))
          }
          case (IsClosing, (j, true) :: t) => {
            numClosing +=1
            (idx, true) :: stack
          }
          case (IsClosing, (j, false) :: t) => {
            egypticityTypeArray(idx) = SameLineClosing
            egypticityTypeArray(j) = SameLineOpening
            numOpening -= 1
            // do not increase numClosing, it stays the same
            t
          }
          case (IsOpening, s) => {
            numOpening += 1
            (idx, false) :: s
          }
          // $COVERAGE-OFF$
          case (_, _) =>
            throw new Error("unreachable, excluded by enclosing `if`")
          // $COVERAGE-ON$
        }
      }
    }
    for (((idx, isCl), i) <- stack.zipWithIndex) {
      egypticityTypeArray(idx) = if (isCl) {
        EgyptianClosing(i - numOpening)
      } else {
        EgyptianOpening(numOpening - 1 - i)
      }
    }
    egypticityTypeArray
  }

  def apply[TT](
    tokens: List[Token[TT]],
    hints: EgyptianBracketsHints[TT]
  ): EgyptianBrackets = {
    import collection.mutable.ArrayBuilder
    val descriptorsBldr = new ArrayBuilder.ofLong
    val tokenToLineBldr = new ArrayBuilder.ofInt
    val egypticityBldr =  new ArrayBuilder.ofRef[Egypticity]
    var lineIdx = 0

    foreachLine[Token[TT]](
      tokens,
      tok => hints.isLineBreak(tok.tokenType),
      line => {
        // TODO: there can be more than one whitespace token at start of line!
        val indentationDescriptor = line
          .headOption
          .filter(t => hints.isIndentationWhitespace(t.tokenType))
          .map(t => WhitespaceHashing.hash(t.stringSlice))
          .getOrElse(EmptyLineHash)
      
        descriptorsBldr += indentationDescriptor
      
        val e = lineEgypticity(line, hints)
        for (x <- e) {
          tokenToLineBldr += lineIdx
          egypticityBldr += x
        }
      },
      lineSep => {
        tokenToLineBldr += lineIdx
        egypticityBldr += EgyptianBrackets.Neutral
        lineIdx += 1
      }
    )

    new EgyptianBrackets(
      descriptorsBldr.result,
      tokenToLineBldr.result,
      egypticityBldr.result
    )
  }
}
