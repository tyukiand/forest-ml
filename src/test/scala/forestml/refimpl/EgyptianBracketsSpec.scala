package forestml.refimpl

import org.scalatest._

class EgyptianBracketsSpec extends FlatSpec with Matchers {

  import EgyptianBrackets._
  "you" should "add some EgyptianBrackets tests again" in {}
  /*
  "linewise mapping" should "work on small hand-crafted examples" in {
    mapLinewise[Int, Int](
      List(3, 6, 9, -1, 12, 24, 48, 400, -1, 5, 25, 15, 35, 45),
      _ == -1,
      l => { val n = l.size; l.map(_ / n) },
      _ => -100
    ) should be(List(1, 2, 3, -100, 3, 6, 12, 100, -100, 1, 5, 3, 7, 9))
  
    mapLinewise[Char, Char](
      "hello\nworld\nfoo\nbar\nbaz".toList,
      _ == '\n',
      l => l.reverse,
      identity
    ).mkString should be("olleh\ndlrow\noof\nrab\nzab")
  }

  it should "work on randomized examples" in {
    val rng = new util.Random
    for (_ <- 1 to 100) {
      val n = rng.nextInt(20)
      val lines = List.fill(n){rng.alphanumeric.take(rng.nextInt(30)).mkString}
      val s = lines.mkString("\n")
      val a = mapLinewise[Char, Char](
        s.toList,
        _ == '\n',
        _.sorted,
        identity
      ).mkString
      val b = lines.map(_.sorted).mkString("\n")
      a should be(b)
    }
  }

  "egypticity computation" should "work on hand-crafted examples" in {

    val noWhitespaceHash = WhitespaceHashing.hash(StringSlice.whole(""))

    def ec(i: Int) = EgyptianClosing(i)
    def eo(i: Int) = EgyptianOpening(i)
    def so = SameLineOpening
    def sc = SameLineClosing
    def n = Neutral

    for {
      (example, expected) <- List(
        (
          ")(()(",
          List(ec(0), eo(0), so, sc, eo(1))
        ),
        (
          "))(()(()())(",
          List(ec(1), ec(0), eo(0), so, sc, so, so, sc, so, sc, sc, eo(1))
        ),
        (
          "(a()(",
          List(eo(0), n, so, sc, eo(1))
        )
      )
    } {
      val tokens = example.zipWithIndex.map{case (c, i) => Token[Char](
        StringSlice.whole(example).slice(i, i + 1),
        c,
        i
      )}.toList
      def isWhitespace(t: Char) = false // no whitespace in this test
      def isOpening(t: Char) = t == '('
      def isClosing(t: Char) = t == ')'
      val res = lineEgypticity(tokens, isWhitespace, isOpening, isClosing)
      res should contain theSameElementsAs expected
    }

  }
  */

}
