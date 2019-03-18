package forestml.refimpl

import org.scalatest._

class WhitespaceHashSpec extends FlatSpec with Matchers {

  val rng = new util.Random(0)

  def randomWhitespace(numBlocks: Int): String = {
    val wsChars = " \t\u3000"
    val res = (for (b <- 1 to numBlocks) yield {
      val c = wsChars(rng.nextInt(3))
      new String(Array.fill(rng.nextInt(15) + 1)(c))
    }).mkString
    assert(countBlocks(res) <= numBlocks)
    res
  }

  def countBlocks(s: String): Int = {
    if (s.isEmpty) 0
    else {
      val f = s.head
      countBlocks(s.dropWhile(_ == f)) + 1
    }
  }

  def describe(s: String): String =
    s.codePoints.toArray.map(x => f"${x}%04x").mkString(",")

  def checkExactReconstruction(w: String) = {
    val h = WhitespaceHashing.hash(StringSlice.whole(w))
    val v = WhitespaceHashing.decode(h)
    withClue(
      "Original:    " + describe(w) + 
      "\n             " + w
        .replaceAll(" ", "s")
        .replaceAll("\t", "t")
        .replaceAll("\u3000", "U") + 
      "\nReconstruct: " + describe(v) +
      "\n             " + v
        .replaceAll(" ", "s")
        .replaceAll("\t", "t")
        .replaceAll("\u3000", "U") + 
      "\nhash: " + h.toBinaryString +
      "\nhelp: " + "H" * (h.toBinaryString.size - 48) + "^^^^XX" * 8 + 
      "\n"
    ) {
      v should be(w)
    }
  }

  "WhitespaceHash" should "reconstruct exactly for up to 8 blocks" in {
    for (b <- 0 to 8) {
      for (_ <- 0 until 100) {
        checkExactReconstruction(randomWhitespace(b))
      }
    }
  }

  it should "reconstruct exactly for up to 120 equal chars" in {
    for (c <- " \t\u3000") {
      for (i <- 0 until 120) {
        checkExactReconstruction(c.toString * i)
      }
    }
  }

  it should (
    "degrade gracefully into " + 
    "quasi-random hash function for up to 100 blocks"
  ) in {
    for (i <- 0 to 10) {
      val n = 100
      val blocks = List.fill(n)(randomWhitespace(1))
      val strings = (for (ts <- blocks.tails) yield ts.mkString).toSet
      val hashes = strings.map {
        s => WhitespaceHashing.hash(StringSlice.whole(s))
      }.toSet
  
      strings.size should be(n + 1) // +1 for empty
      hashes.size should be(n + 1)
    }
  }

}
