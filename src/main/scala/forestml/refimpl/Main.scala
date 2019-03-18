package forestml.refimpl

import flow._
/*
object Main {
  
  def main(args: Array[String]): Unit = {

    locally {
      val parser = (EgyptianBracketsParserGenerator
        .generateParser(ForestMlGrammar, ForestMlBracketHints)
        (ForestMlGrammar.root)
      )
  
      val juxtapositionExamples = List(
        "\"\"",
        "\"x\"",
        "\"a b\"",
        "\"/* that's a verbatim block */  /!/* not a comment */ %u(1234)!/\"",
        "\"/* that's an unicode escape */ %u(CAFE)\"",
        "\"/* that's decimal unicode escape */ %(10)\"",
        "\"/* that's just text */ text\"",
        "\"text and more text\"",
        "  \"text and %u(1234) unicode escape  \"",
        "hey + hou",
        "hello + \"world\"",
        "        hello  /* comment on plus */ + /* cmt */\"world\"",
        "a + b + c /*?*/ d + e + /*!*/f"
      )

      val treeDefExamples = List(
        "/t()",
        "/t(a)",
        "/*stuff before treeDef*/ / /*name*/t    /*children*/( a  )",
        "/tree()",
        "/a + b()",
        """  / "a" + a + "hello world" ("child1" "child2")""",
        "/a(/b(c) /d(e) f)",
        """ /treeName"hello /emph(world)" """
      )

      val fileExamples = for {
        f <- List(
        )
      } yield io.Source.fromFile(f).getLines.mkString("\n")

      val examples = juxtapositionExamples ++ treeDefExamples ++ fileExamples

      for (example <- examples) {
        println("================")
        println(example)
        println("----------------")
        val lexResult = ForestMlLexer.tokenize(example)
        if (lexResult._2.nonEmpty) {
          println("LEXICAL ERRORS!")
          lexResult._2 foreach println
        }
        val tokens = lexResult._1
     
        val (pErrs, cstOpt) = parser.parse(tokens)

        if (pErrs.nonEmpty) {
          println("PARSE ERRORS!")
          pErrs foreach println
        }
        println(cstOpt)
        println(cstOpt.map(_.toCode))
      }
    }
  }

}
*/
