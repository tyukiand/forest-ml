package forestml.refimpl

import org.scalatest._

/** Tests [Lexer], [LexerInput], [LexerAction], [LexerActions], 
  * without any specific details of ForestML
  */
class LexerSpec extends FlatSpec with Matchers {

  "LexerActions" should "support `peekSwitch`" in {

    val lex = new Lexer with LexerActions {
      type TT = String
      lazy val token = peekSwitch(
        Eof -> cannotHappen("honestly..."),
        is('a') -> { advance(1) andThen emit("A") },
        is('b') -> { advance(1) andThen emit("B") },
        is('(') -> { advance(1) andThen emit("L") },
        is(')') -> { advance(1) andThen emit("R") },
        Default -> { err("id", "?") andThen advance(1) andThen skip }
      )
    }

    val input = "(ab)(a(b))xx(x)"
    val expct = "LABRLALBRRLR"
    val (toks, errs) = lex.tokenize(input)
    errs.size should be(3)
    toks.map(_.tokenType).mkString should be(expct)
  }

  it should "invoke `peekSwitch` `eofHandler` on end of input" in {
    val lex = new Lexer with LexerActions {
      type TT = String
      lazy val token = advance(1) andThen peekSwitch(
        Eof -> emit("."),
        Default -> { advance(1) andThen emit("^x") }
      )
    }

    val input = ";a;a;a;"
    val expct = "^x^x^x."
    val (toks, errs) = lex.tokenize(input)
    errs.size should be(0)
    toks.map(_.tokenType).mkString should be(expct)
  }

  it should "support `rep`" in {
    val lex = new Lexer with LexerActions {
      type TT = String
      lazy val token = peekSwitch(
        Eof -> cannotHappen("honestly..."),
        is('a') -> { rep(_ == 'a') andThen emit("A") },
        is('b') -> { rep(_ == 'b') andThen emit("B") },
        Default -> { err("id", "?") andThen advance(1) andThen skip }
      )
    }

    val input = "aaa!!aaabbbbbbaaabbbb!bb!bbbaaaaa!bbbb"
    val expct = "AABABBBAB"
    val (toks, errs) = lex.tokenize(input)
    errs.size should be(5)
    toks.map(_.tokenType).mkString should be(expct)
  }

  it should "find the most specific character class with `joinRep1`" in {
    val lex = new Lexer with LexerActions {
      type TT = String
      lazy val token = advance(1) andThen joinRep1(
        is('1') -> "1",                // ones
        pred(_.isDigit) -> "N",        // numeric
        pred(_.isLetterOrDigit) -> "A" // alphanumeric
      )
    }

    val input = ";1;11;1111;12;7583;a42;42a;1x2b;11;543;stop"
    val expct = "111NNAAA1NA"
    val (toks, errs) = lex.tokenize(input)
    errs.size should be(0)
    toks.map(_.tokenType).mkString should be(expct)
  }

  it should "find the next occurrence using `SeekNextOccurrence`" in {
    // "seeking end of comment"-examples
    for (
      (exText, exSub, lenHint, expectedRes) <- List(
        ("hello world /* foo */ blah", "*/",
         "^^^^^^^^^^^^^^^^^^^", true
        ),
        ("hello world /* bar /+/*/ blah*/", "*/",
         "^^^^^^^^^^^^^^^^^^^^^^", true
        ),
        ("hello */", "*/",
         "^^^^^^", true
        ),
        ("hello world *", "*/",
         "", false
        ),
        ("/** /* */*///**/*/*/", "**/",
         "^^^^^^^^^^^^^", true
        )
      )
    ) {
      val i = LexerInput.fromString(exText)
      val (res, i2, _, _) = SeekNextOccurrence[String](exSub).apply(
        i,
        AccumulatorBuilder.empty[Token[String]],
        AccumulatorBuilder.empty[SyntaxError]
      )
      res should be(expectedRes)
      i2.skip.position should be(lenHint.size)
    }
  }

  it should "find the next occurrence using `seekNext` syntax" in {

    val lex = new Lexer with LexerActions {
      type TT = String
      lazy val token = 
        for {
          ok <- seekNext("STOP")
          _ <- if (ok) {
            emit("TEXT") andThen 
            advance(4) andThen
            emit("STOP")
          } else {
            err("no-id", "unfinished 'STOP'") andThen 
            advance(1) andThen
            skip
          }
        } yield ()
    }

    val expctTypes = List("TEXT", "STOP", "TEXT", "STOP", "TEXT", "STOP")
    val expctLexemes = List(
      "PSOThelloSTOphelloSToHello", "STOP", 
      "Stohasruhthuaaxb", "STOP",
      "p6ufic/urfkcxs89g33450y", "STOP"
    )
    val errorSuffix = "errorsSTO"
    val input = expctLexemes.mkString + errorSuffix
    val (toks, errs) = lex.tokenize(input)
    errs.size should be(errorSuffix.size)
    toks.map(_.tokenType) should be(expctTypes)
    toks.map(_.stringSlice.getString) should be(expctLexemes)
  }

  it should "`map` results" in {

    // somewhat artificial "compute string length in funny way"-example.
    // Token type is used to return the integer.
    val lex = new Lexer with LexerActions {
      type TT = Int
      lazy val token = advance(1) andThen (for {
        len <- strLen
        _ <- emit(len)
      } yield ())
      lazy val strLen = rep(_ == 'x')
    }

    val input = ";xx;xxx;xxxxx;xxxxxxx;xxxxxxxxxxx"
    val expct = List(2, 3, 5, 7, 11)
    val (toks, errs) = lex.tokenize(input)
    errs.size should be(0)
    toks.map(_.tokenType) should be(expct)
  }

  it should "do nothing when using `pure` and `unit`" in {

    val lex = new Lexer with LexerActions {
      type TT = String
      lazy val token = 
        advance(1) andThen pure("ignored") andThen unit andThen emit("*")
    }

    val input = "abcde"
    val expct = "*****"
    val (toks, errs) = lex.tokenize(input)
    errs.size should be(0)
    toks.map(_.tokenType).mkString should be(expct)
  }

  it should "throw error if `cannotHappen` does happen" in {
    val lex = new Lexer with LexerActions {
      type TT = String
      lazy val token = cannotHappen("expected empty input")
    }

    val input = "not empty (troll!)"
    an [Error] should be thrownBy lex.tokenize(input)
  }

}
