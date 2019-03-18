package forestml.refimpl

object WhitespaceHashing {
  val CharBits = 2 // Space, Tab, CJK, sth. else
  val RepsBits = 4 // 16x is long enough!
  val MaxReps = 15 // max repetitions per block
  val BlocksBits = 48 // space exclusively for RLE-blocks

  def hash(s: StringSlice): Long = {

    var checksum = 17
    var currentChar: Char = '\uFFFF'
    var reps = 0
    var rleBlockMultiplier: Long = 1L
    var result: Long = 0
    var blockIdx = 0
    var overflow = false
    val n = s.size

    for (i <- 0 to n) {
      val c = if (i < n) s(i) else '$'
      checksum = checksum * 31 + c
      if (c == currentChar && reps < MaxReps) {
        reps += 1
      } else if (currentChar == '\uFFFF') {
        currentChar = c
        reps = 1
      } else {
        val currentCharClassification = currentChar match {
          case ' ' => 0
          case '\t' => 1
          case '\u3000' => 2
          // $COVERAGE-OFF$
          case _ => 3
          // $COVERAGE-ON$
        }
        val block = currentCharClassification + 4 * reps
        result ^= rleBlockMultiplier * block
        rleBlockMultiplier = if (blockIdx > 7 && c != '$') {
          // overflow, exact representation does not longer
          // fit into the long, trash everything in a fun way!
          overflow = true
          1.toLong << (blockIdx % 60)
        } else {
          // allocate whole new block
          rleBlockMultiplier << (2 + RepsBits)
        }
        blockIdx += 1
        currentChar = c
        reps = 1
      }
    }
    if (overflow) {
      result ^= checksum.toLong << 32
    } else {
      // compressing of `checksum` depends on BlockBits,
      // the result must fit into 64-BlockBits !
      checksum ^= checksum >> 16
      result ^= (checksum.toLong << BlocksBits)
    }
    result
  }

  /** Decoder that attempts to reconstruct the original whitespace string
    * from the hash. It works if the whitespace contained up to 8 blocks
    * of up to 15 characters ' ', '\t', and '\u3000'.
    *
    * For test purposes only.
    */
  private[refimpl] def decode(h: Long): String = {
    var hRest = h
    var result = ""
    for (i <- 0 until 8) {
      val c = (hRest & 3) match {
        case 0 => ' '
        case 1 => '\t'
        case 2 => '\u3000'
        // $COVERAGE-OFF$
        case _ => ' ' // some other whitespace... do "something"
        // $COVERAGE-ON$
      }
      val reps = ((hRest >> 2) & 0xF /* RepsBits mask */).toInt
      hRest = hRest >> (2 + RepsBits)
      result += c.toString * reps
    }
    result
  }
}
