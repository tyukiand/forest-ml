package forestml.refimpl

// Seems like this class can be eliminated altogether on a JVM,
// I'll keep it anyway, just to know exactly which functionality
// of `String` I'm using, and ensuring that I don't accidentally
// rely on the Java/Scala stuff too much.

/** Slice of the `original` string.
  *
  * Represents a slice of the input without duplicating the character data in 
  * memory.
  */
case class StringSlice(original: String, start: Int, end: Int) {
  def apply(i: Int): Char = original(start + i)
  def getString: String = original.slice(start, end)
  def size: Int = end - start
  def slice(startIncl: Int, endExcl: Int): StringSlice =
    StringSlice(original, start + startIncl, start + endExcl)

  // $COVERAGE-OFF$
  // (don't rely on any particular representation, 
  // use `getString` instead)
  override def toString = getString
  // $COVERAGE-ON$
}

object StringSlice {
  def whole(str: String): StringSlice = StringSlice(str, 0, str.size)

  def stripMargin(slice: StringSlice, delim: Char = '|')
  : List[(StringSlice, StringSlice)] = {

    // State machine sketched in CS-XIVp112

    val n = slice.size
    val ScanningPrefix = 0
    val ScanningText = 1
    val ScannedCr = 2
    val Eof = '\uFFFF'

    var revAcc: List[(StringSlice, StringSlice)] = Nil
    var prefix: StringSlice = null

    var state = ScanningPrefix
    var lastSliceEnd = 0
    var running = true
    var idx = 0

    @inline def emitPrefix = {
      prefix = slice.slice(lastSliceEnd, idx)
      lastSliceEnd = idx
    }

    @inline def emitText = {
      val txt = slice.slice(lastSliceEnd, idx)
      revAcc ::= (prefix, txt)
      prefix = null
      lastSliceEnd = idx
    }

    while (running) {
      val c = if (idx < n) slice(idx) else Eof

      state match {
        case ScanningPrefix => c match {
          case '\r' => emitPrefix; idx += 1; state = ScannedCr
          case '\n' => emitPrefix; idx += 1; emitText
          case w if w.isWhitespace => idx += 1
          case `delim` => idx += 1; emitPrefix; state = ScanningText
          case Eof => emitPrefix; emitText; running = false
          case _ => emitPrefix; idx += 1; state = ScanningText
        }
        case ScannedCr => c match {
          case '\r' => emitText; emitPrefix; idx += 1
          case '\n' => idx += 1; emitText; state = ScanningPrefix
          case w if w.isWhitespace => 
            emitText; idx += 1; state = ScanningPrefix
          case `delim` => emitText; idx += 1; emitPrefix; state = ScanningText
          case Eof => emitText; running = false
          case _ => emitText; emitPrefix; idx += 1; state = ScanningText
        }
        case ScanningText => c match {
          case '\r' => idx += 1; state = ScannedCr
          case '\n' => idx += 1; emitText; state = ScanningPrefix
          case Eof => emitText; running = false
          case _ => idx += 1
        }
      }
    }

    revAcc.reverse
  }

}
