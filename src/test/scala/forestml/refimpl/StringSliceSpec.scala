package forestml.refimpl

import org.scalatest._

class StringSliceSpec extends FlatSpec with Matchers {

  "StringSlice" should 
  "represent the substring defined by inclusive-exclusive indices" in {
    val s = "hello, world"
    StringSlice(s, 0, 5).getString should be("hello")
    StringSlice(s, 7, 12).getString should be("world")
    StringSlice(s, 6, 6).getString should be("")
    StringSlice(s, 0, s.size).getString should be(s)
    StringSlice(s, 12, 12).getString should be("")
  }

  it should "extract subslices" in {
    val s = "hello, world"
    StringSlice(s, 7, 12).slice(1, 4).getString should be("orl")

    for (i <- 0 until s.size; j <- i to s.size) {
      StringSlice.whole(s).slice(i, j).getString should be(s.slice(i, j))
    }
  }

  // Warning! The output for this test might seem broken in SBT, because
  // we have to deal with `\r`, and it tends to trash the output quite a bit!
  it should "strip margin (hand crafted examples)" in {
    import StringSlice.{apply => ss}
    val examples = List[(String, String => List[(StringSlice, StringSlice)])](
      // empty string counts as single "line" with empty prefix and empty text
      ("", s => List((ss(s, 0, 0), ss(s, 0, 0)))),
      (" ", s => List((ss(s, 0, 1), ss(s, 1, 1)))),
      ("\r", s => List((ss(s, 0, 0), ss(s, 0, 1)))),
      ("\rx", s => List(
        (ss(s, 0, 0), ss(s, 0, 1)),
        (ss(s, 1, 1), ss(s, 1, 2))
      )),
      ("\r\r", s => List(
        (ss(s, 0, 0), ss(s, 0, 1)),
        (ss(s, 1, 1), ss(s, 1, 2))
      )),
      ("\r\r |a", s => List(
        (ss(s, 0, 0), ss(s, 0, 1)),
        (ss(s, 1, 1), ss(s, 1, 2)),
        (ss(s, 2, 4), ss(s, 4, 5))
      )),
      ("\r\n |a", s => List(
        (ss(s, 0, 0), ss(s, 0, 2)),
        (ss(s, 2, 4), ss(s, 4, 5))
      )),
      ("   xyz\nabc", s => List(
        (ss(s, 0, 3), ss(s, 3, 7)),
        (ss(s, 7, 7), ss(s, 7, 10))
      )),
      (" \na", s => List(
        (ss(s, 0, 1), ss(s, 1, 2)),
        (ss(s, 2, 2), ss(s, 2, 3))
      )),
      ("  \r|hey", s => List(
        (ss(s, 0, 2), ss(s, 2, 3)),
        (ss(s, 3, 4), ss(s, 4, 7))
      )),
      ("blah\rblah", s => List(
        (ss(s, 0, 0), ss(s, 0, 5)),
        (ss(s, 5, 5), ss(s, 5, 9))
      ))
    )

    for ((i, res) <- examples) {
      val slices = res(i)
      val k = slices.map{ case (p, t) => p.getString + t.getString }.mkString
      withClue(
        s"Test itself is broken: ${i.codePoints.toArray.mkString(",")}"
      ) {
        k should be(i) // testing the test itself, not the implementation!
      }

      val actualSlices = StringSlice.stripMargin(StringSlice.whole(i))
      withClue(
        s"code points = `${i.codePoints.toArray.mkString(",")}`" + 
        "actual res: " + actualSlices.map{ case (p, t) => 
          "PREF=`" + p.getString.codePoints.toArray.mkString(",") + 
          "` TEXT = `" + t.getString.codePoints.toArray.mkString(",") + "`"
        }.mkString(";")
      ) {

        // now we test the implementation
        actualSlices should be(slices)
      }
    }
  }

  lazy val whitespaceChars = 
    (0 until '\uFFFE').map(_.toChar).filter{ c =>
      c.isWhitespace && c != '\n' && c != '\r'
    }.toArray

  it should "strip margin (randomized)" in {
    val rng = new util.Random(42)
    for (i <- 0 until 100) {
      val archaicMacLineBreaks = rng.nextBoolean
      val numLines = rng.nextInt(20) + 1

      val lines = for (i <- 0 until numLines) yield {
        val prefix = if (rng.nextDouble < 0.1) {
          ""
        } else {
          val wsLen = rng.nextInt(10)
          val ws = (1 to wsLen)
            .map(_ => whitespaceChars(rng.nextInt(whitespaceChars.size)))
            .mkString
          if (rng.nextDouble < 0.5) {
            ws 
          } else {
            ws + "|"
          }
        }
        val textWithoutLineBreak =
          rng.alphanumeric.take(rng.nextInt(20)).mkString
        val lineBreak = 
          if (archaicMacLineBreaks) "\r"
          else if (rng.nextDouble < 0.5) "\r\n"
          else "\n"
        (prefix, textWithoutLineBreak + lineBreak)
      }

      val garbage0 = rng.alphanumeric.take(rng.nextInt(10)).mkString
      val verbString = lines.map{ case (p, t) => p + t }.mkString
      val garbage1 = rng.alphanumeric.take(rng.nextInt(10)).mkString

      val s0 = StringSlice.whole(garbage0 + verbString + garbage1)
      val s1 = s0.slice(garbage0.size, garbage0.size + verbString.size)
      
      val computedMargin = StringSlice.stripMargin(s1)

      for ((((x, y), (v, w)), i) <- (lines zip computedMargin).zipWithIndex) {
        withClue(
          "Lines:\n  " + 
          lines.map { case (p, t) => 
            p.codePoints.toArray.mkString(",") + ";" +
            t.codePoints.toArray.mkString(",")
          }.mkString("\n  ") +
          "\nMargin:\n  " +
          computedMargin.map { case (p, t) => 
            p.getString.codePoints.toArray.mkString(",") + ";" +
            t.getString.codePoints.toArray.mkString(",")
          }.mkString("\n  ") + 
          "\nWrong margin computed in line " + i
        ) {
          v.getString should be(x)
          w.getString should be(y)
        }
      }
    }
  }

}
