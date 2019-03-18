package forestml.refimpl


object ForestMlGrammar
extends Grammar[ForestMlTokenType, List[Token[GenWhitespace]]] {

  import ForestMlCst.{
    IndentationBarrier => CstIndentationBarrier,
    Whitespace => CstWhitespace,
    EscapeEscape => CstEscapeEscape,
    _
  }

  def isWhitespace(t: ForestMlTokenType) = t.isInstanceOf[GenWhitespace]

  /** Converts list of whitespace tokens into whitespace in the
    * sense of CST.
    *
    */
  def convertWs(genWsToks: List[Token[GenWhitespace]]): Ws = {
    // Requires some cheap "quasi-parsing" to glue the triples
    // of comment tokens into a single comment.
    import collection.mutable.ListBuffer
    val buffer = ListBuffer.empty[WhitespaceOrComment]
    @annotation.tailrec
    def rec(rest: List[Token[GenWhitespace]]): Unit = rest match {
      case Nil => ()
      case h :: t => {
        val (cst, rest) = h.tokenType match {
          case Whitespace => (CstWhitespace(h.stringSlice), t)
          case LeftCommentDelim => {
            val a :: b :: s = t
            val reinforcement = h.stringSlice.size - 1
            (Comment(reinforcement, a.stringSlice), s)
          }
          case CommentBody => {
            throw new Error("Impossible") // TODO: impossible ID?
          }
          case RightCommentDelim => {
            throw new Error("Impossible") // TODO: impossible ID?
          }
          case LineBreak => (CstWhitespace(h.stringSlice), t)
          case IndentationBarrier => 
            (CstIndentationBarrier(h.stringSlice), t)
        }
        buffer += cst
        rec(rest)
      }
    }
    rec(genWsToks)
    Ws(buffer.result)
  }

  lazy val unicodeDecimalEscape: L[UnicodeDecimalEscape] = inlined {
    (
      EscapeUnicodeDecLeft ~
      CharsDecimal.ensuring(
        _._1.isEmpty,
        "No whitespace / comments allowed inside unicode escapes."
      ) <~
      RightParen.ensuring(
        _._1.isEmpty,
        "No whitespace / comments allowed inside unicode escapes."
      )
    ) ^^ {
      case ((ws, _), (_, num)) =>
      UnicodeDecimalEscape(convertWs(ws), num.stringSlice)
    }
  }


  lazy val unicodeHexEscape: L[UnicodeHexEscape] = inlined {
    (
      EscapeUnicodeHexLeft ~
      (CharsDecimal | CharsHexadecimal).ensuring(
        _._1.isEmpty,
        "No whitespace / comments allowed inside unicode escapes."
      ) <~
      RightParen.ensuring(
        _._1.isEmpty,
        "No whitespace / comments allowed inside unicode escapes."
      )
    ) ^^ {
      case ((ws, _), (_, num)) =>
      UnicodeHexEscape(convertWs(ws), num.stringSlice)
    }
  }

  lazy val verbatim: L[VerbatimBlock] = rule("verbatim") {
    // Note: tokenizer always emits `CharsGeneral`
    (LeftVerbDelim ~ CharsGeneral <~ RightVerbDelim) ^^ {
      case ((ws, delim), (_, body)) =>
      val reinforcement = delim.stringSlice.size - 2
      VerbatimBlock(convertWs(ws), reinforcement, body.stringSlice)
    }
  }

  lazy val identifier: L[UnquotedIdentifier] = inlined {
    (CharsDecimal | CharsHexadecimal | CharsIdentifier) ^^ {
      case (ws, s) => UnquotedIdentifier(convertWs(ws), s.stringSlice)
    }
  }

  /* raw character data inside marked up text */
  lazy val sliceFragment: L[SliceFragment] = inlined {
    (CharsDecimal | CharsHexadecimal | CharsIdentifier | CharsGeneral) ^^ {
      case (ws, cs) => SliceFragment(convertWs(ws), cs.stringSlice)
    }
  }

  lazy val markupFragment: L[MarkupFragment] = rule("markupFragment"){
    sliceFragment |
    unicodeHexEscape |
    unicodeDecimalEscape |
    (verbatim ^^ EmbeddedVerbatim) |
    embeddedTree |
    nondisruptive
  }

  lazy val quotedMarkup: L[QuotedMarkup] = inlined {
    (DoubleQuote ~ (markupFragment.*) ~ DoubleQuote) ^^ {
      case (((ws1, _), fragments), (ws2, _)) =>
        QuotedMarkup(convertWs(ws1), fragments, convertWs(ws2))
    }
  }

  lazy val markup: L[Markup] = rule("markup") { 
    identifier | quotedMarkup | verbatim
  }

  lazy val markupSum: L[MarkupSum] = inlined {
    (markup ~ (Plus ~ markup).*) ^^ {
      case (head, tail) => {
        val (plusWhitespace, markupTail) = tail.map {
          case ((plusWs, _), m) => (plusWs, m)
        }.unzip
        MarkupSum(head :: markupTail, plusWhitespace map convertWs)
      }
    }
  }

  lazy val juxtaposable: L[Juxtaposable] = rule("juxtaposable") {
    markupSum | (treeDef ^^ JuxtaposableTreeDef)
  }


  lazy val juxtaposition: L[Juxtaposition] = rule("juxtaposition") {
    juxtaposable.* ^^ Juxtaposition
  }

  lazy val parenthesizedJuxtaposition: L[ParenthesizedJuxtaposition] = inlined {
    (LeftParen ~ juxtaposition ~ RightParen) ^^ {
      case (((lWs, _), j), (rWs, _)) => 
        ParenthesizedJuxtaposition(convertWs(lWs), j, convertWs(rWs))
    }
  }

  lazy val treeDef: L[TreeDef] = rule("treeDef") {
    (TreeStart ~ markup) ~
    (
      treeDefMultipartLabelRest |
      (markup ^^ { m => (
        List.empty[(Ws, Markup)], // append nothing to label
        SingleLeafChildNodes(m)
      )}) |
      (treeDef ^^ { t => (
        List.empty[(Ws, Markup)], // append nothing to label
        SingleTreeChildNodes(t)
      )})
    ) ^^ { case (((tWs, _), labelFirstPart), (labelOtherParts, children)) =>
      val (plusWhitespace, markupTail) = labelOtherParts.unzip
      TreeDef(
        convertWs(tWs),
        MarkupSum(labelFirstPart :: markupTail, plusWhitespace),
        children
      )
    }
  }

  /** Rest of a tree definition where the label is defined as a
    * sum of strings.
    *
    * Example:
    * {{{
    * /"first " + "part" + " second part" (ch1 ch2)
    * }}}
    * is a definition of a tree where the label is
    * `"first part second part"`. Everything right after `/"first" `
    * would be matched by this rule.
    *
    * The parsed result consists of a list and a `ChildNodes` element.
    * The list contains whitespace tokens that preceed the plusses,
    * and the markup elements concatenated by those plusses.
    */
  lazy val treeDefMultipartLabelRest
  : L[(List[(Ws, Markup)], ChildNodes)] = inlined {
    ((Plus ~ markup).* ~ parenthesizedJuxtaposition) ^^ {
      case (plussesNameSnippets, jxt) => {
        val p = plussesNameSnippets.map {
          case ((plusWs, _), m) => (convertWs(plusWs), m)
        }
        (p, ParenthesizedChildNodes(jxt))
      }
    }
  }

  lazy val embeddedTree: L[EmbeddedTree] = inlined { treeDef ^^ EmbeddedTree }

  lazy val nondisruptive: L[MarkupFragment] = inlined {
    (LeftParen ^^ { case (ws, _) => LeftParenOrdinaryText(convertWs(ws)) }) |
    (RightParen ^^ { case (ws, _) => RightParenOrdinaryText(convertWs(ws)) }) |
    (Plus ^^ { case (ws, _) => PlusOrdinaryText(convertWs(ws)) }) |
    (EscapeEscape ^^ { case (ws, _) => CstEscapeEscape(convertWs(ws)) }) |
    (EscapeDoubleQuote ^^ { case (ws, _) => DoubleQuoteEscape(convertWs(ws)) }) |
    (EscapeDisruptor ^^ { case (ws, _) => DisruptorEscape(convertWs(ws)) }) |
    (EscapeNewline ^^ { case (ws, _) => NewlineEscape(convertWs(ws)) }) |
    (EscapeCarriageReturn ^^ { 
      case (ws, _) => CarriageReturnEscape(convertWs(ws)) }
    ) |
    (EscapeTab ^^ { case (ws, _) => TabEscape(convertWs(ws)) }) |
    (EscapeBackspace ^^ { case (ws, _) => BackspaceEscape(convertWs(ws)) })
  }

  lazy val root: L[Root] = (juxtaposition ~ EndOfInput) ^^ {
    case (jxt, ws) => Root(jxt, convertWs(ws))
  }
}
