package forestml.refimpl

import util.{Either, Left, Right}

/** Concrete syntax tree of the Forest-ML.
  *
  * Every element of the concrete syntax tree carries around a
  * (possibly empty) prefix consisting of alternating whitespace and
  * comment tokens, so that all the indentation can be reconstructed
  * exactly from the CST.
  */
object ForestMlCst {

  sealed trait WhitespaceOrComment {
    def toCode: String
  }

  case class Whitespace(slice: StringSlice) extends WhitespaceOrComment {
    def toCode = slice.getString
  }

  case class Comment(reinforcement: Int, slice: StringSlice) 
  extends WhitespaceOrComment {
    def toCode = {
      "/" + "*" * reinforcement +
      slice.getString +
      "*" * reinforcement + "/"
    }
  }

  case class IndentationBarrier(slice: StringSlice)
  extends WhitespaceOrComment {
    def toCode = slice.getString
  }

  case class Ws(whitespaceOrComments: List[WhitespaceOrComment]) {
    def toCode = whitespaceOrComments.map(_.toCode).mkString
  }


  /** Juxtaposition of tree definitions and concatenated text snippets
    * that forms a forest.
    *
    * For example:
    * {{{
    *    /treeDef() "text" "marked up /emph(text)" "summand 1" + "summand 2"
    * }}}
    * is a juxtaposition of four elements:
    * $ - `/treeDef()` is a standalone tree definition,
    * $ - `"text"` is a piece of text without markup
    * $ - `"marked up /emph(text)"` is a piece of text with markup
    * $ - `"summand 1" + "summand 2"` is a sum of two text snippets
    *
    * Whitespace and comments are attached to the elements of the
    * juxtaposition.
    */
  case class Juxtaposition(elems: List[Juxtaposable]) {
    def toCode = elems.map(_.toCode).mkString
  }


  /** Either standalone tree definitions, or sums of marked up text snippets. */
  sealed trait Juxtaposable {
    def toCode: String
  }

  /** Tree definition that occurs in a juxtaposition of elements that
    * together form a forest.
    *
    * For example, the `/child()` tree in
    * {{{
    *   /parent(
    *     /child()
    *     "some text"
    *     /anotherChild()
    *   )
    * }}}
    * is a tree definition that appears as a part of juxtaposition.
    *
    * Whitespace and comments are attached to the `TreeDef`.
    */
  case class JuxtaposableTreeDef(treeDef: TreeDef)
  extends Juxtaposable {
    def toCode = treeDef.toCode
  }


  /** Nonempty sum of text pieces. The text pieces can be marked up.
    *
    * For example,
    *
    * {{{
    * "foo bar" + "baz /embedded(tree) more text"
    * }}}
    *
    * is a `MarkupSum` with two summands.
    *
    * The whitespace and comments between a text piece and the `+` are
    * stored in the `plusWhitespace` list, which has one less element than
    * there are text pieces. All the other whitespace and comments are 
    * attached to the summands.
    */
  case class MarkupSum(summands: List[Markup], plusWhitespace: List[Ws])
  extends Juxtaposable {
    require(summands.nonEmpty, "MarkupSum summand list must be nonempty")
    def toCode = {
      (plusWhitespace.map(_.toCode) zip summands.tail.map(_.toCode)).map {
        case (w, s) => w + "+" + s
      }.mkString(summands.head.toCode, "", "")
    }
  }

  /** Superclass for text-like elements - quoted or unquoted, with 
    * markup or without (somewhat of a red-herring, named by the most 
    * general class of elements).
    */
  sealed trait Markup {
    def toCode: String
  }

  /** Quoted piece of text, possibly with embedded markup elements.
    *
    * Things that look like quoted string literals (which could be marked up).
    *
    * Example:
    *
    * {{{
    * "just text %n %(1234) /some(markup) /some(more markup) end"
    * }}}
    *
    * is a piece of text that contains two escape sequences (line break and
    * unicode character `\u04d2`), and two embedded tree definitions
    * (both with label `some`).
    *
    * Four more examples (one `Markup` instance per line):
    * {{{
    *   "some text without markup"
    *   "some text with %(12345) escapes %n"
    *   "some text /emph(with) markup"
    * }}}
    * 
    * Whitespace and comments right before the string literal is stored 
    * in `openingQuoteWs`, whitespace and comments that preceed the closing
    * quote are saved as `closingQuoteWs`. All the other whitespace and 
    * comments are attached to the `fragments`.
    */
  case class QuotedMarkup(
    openingQuoteWs: Ws,
    fragments: List[MarkupFragment],
    closingQuoteWs: Ws
  ) extends Markup {
    def toCode = fragments.map(_.toCode).mkString(
      openingQuoteWs.toCode + "\"",
      "",
      closingQuoteWs.toCode + "\""
    )
  }

  /** Unquoted piece of text without any escapes or markup. */
  case class UnquotedIdentifier(ws: Ws, slice: StringSlice) extends Markup {
    def toCode = ws.toCode + slice.getString
  }

  case class VerbatimBlock(ws: Ws, reinforcement: Int, slice: StringSlice)
  extends Markup {
    def toCode = {
      ws.toCode + 
      "/" + "+" * reinforcement + "!" + 
      slice.getString + 
      "!" + "+" * reinforcement + "/"
    }
  }

  /** Everything that can appear inside a quoted string literal with
    * marked up text.
    */
  sealed trait MarkupFragment {
    def toCode: String
  }


  /** Snippets of character data that are not interpreted in any way,
    * and are simply taken as-is directly from the input source code.
    */
  case class SliceFragment(ws: Ws, stringSlice: StringSlice)
  extends MarkupFragment {
    def toCode = ws.toCode + stringSlice.getString
  }

  /** Escaped unicode code point in hexadecimal notation.
    * 
    * Passed as string with hexadecimal digits, not converted to integer yet.
    */
  case class UnicodeHexEscape(ws: Ws, digits: StringSlice)
  extends MarkupFragment {
    def toCode = ws.toCode + "%u(" + digits.getString + ")"
  }

  /** Escaped unicode code point in decimal notation.
    *
    * Passed as string with decimal digits, not converted to integer yet.
    */
  case class UnicodeDecimalEscape(ws: Ws, digits: StringSlice)
  extends MarkupFragment {
    def toCode = ws.toCode + "%(" + digits.getString + ")"
  }

  case class LeftParenOrdinaryText(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "("
  }
  case class RightParenOrdinaryText(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + ")"
  }
  case class PlusOrdinaryText(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "+"
  }
  case class NewlineEscape(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "%n"
  }
  case class TabEscape(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "%t"
  }
  case class BackspaceEscape(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "%b"
  }
  case class DoubleQuoteEscape(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "%\""
  }
  case class CarriageReturnEscape(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "%r"
  }
  case class DisruptorEscape(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "%/"
  }
  case class EscapeEscape(ws: Ws) extends MarkupFragment {
    def toCode = ws.toCode + "%%"
  }

  /** Tree definition that appears in the middle of marked up text.
    *
    * Example:
    * {{{
    *   "text /tree() text"
    * }}}
    * The `/tree()` is an embedded tree.
    *
    */
  case class EmbeddedTree(treeDef: TreeDef) extends MarkupFragment {
    def toCode = treeDef.toCode
  }

  /** Verbatim block embedded into marked up string literal. */
  case class EmbeddedVerbatim(verbatim: VerbatimBlock) 
  extends MarkupFragment {
    def toCode = verbatim.toCode
  }

  /** Semantically same as `Juxtaposition`, but with additional whitespace
    * and comments right before the enclosing parentheses.
    */
  case class ParenthesizedJuxtaposition(
    openingParenWs: Ws,
    juxtaposition: Juxtaposition,
    closingParenWs: Ws
  ) {
    def toCode = {
      openingParenWs.toCode + "(" + 
      juxtaposition.toCode + 
      closingParenWs.toCode + ")"
    }
  }

  sealed trait ChildNodes {
    def toCode: String
  }

  case class ParenthesizedChildNodes(p: ParenthesizedJuxtaposition)
  extends ChildNodes {
    def toCode = p.toCode
  }
  case class SingleTreeChildNodes(t: TreeDef) extends ChildNodes {
    def toCode = t.toCode
  }
  case class SingleLeafChildNodes(m: Markup) extends ChildNodes {
    def toCode = m.toCode
  }

  /** Definition of a tree, consisting of a label and a forest of child nodes.
    *
    * The label is composed of a sum of markup elements.
    * Note that the abstract syntax tree does not enforce that every part
    * of the label is a simple string without any markup elements.
    * Ensuring that the label is a string (and not a forest itself) is left
    * to a later type checking phase.
    *
    * Example:
    * {{{
    *   /this + " is a " + tree(child1 child2)
    * }}}
    * is a tree definition with label `"this is a tree"` and children
    * `"child1"` and `"child2"` (both simple strings).
    *
    * As explained above,
    * {{{
    *   /"this is /invalid(markup) in the label"(c1 c2)
    * }}}
    * is also a valid `TreeDef`, even though it does not correspond to
    * any valid tree, because the label is not a plain string
    * (it contains markup).
    * Such cases must be filtered out in the later phases.
    */
  case class TreeDef(ws: Ws, labelParts: MarkupSum, childNodes: ChildNodes) {
    def toCode = ws.toCode + "/" + labelParts.toCode + childNodes.toCode
  }

  /** Root node for the entire document.
    * Essentially the same as a `Juxtaposition`, but with
    * an additional whitespace token at the very end.
    */
  case class Root(juxtaposition: Juxtaposition, eofWs: Ws) {
    def toCode = juxtaposition.toCode + eofWs.toCode
  }

}
