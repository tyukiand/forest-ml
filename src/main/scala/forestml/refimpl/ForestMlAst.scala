package forestml.refimpl

object ForestMlAst {

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
    * $ - `"marked up /emph(text)"` is a piece of text ''with'' markup
    * $ - `"summand 1" + "summand 2"` is a sum of two text snippets
    */
  case class Juxtaposition(elems: List[Juxtaposable])


  /** Either standalone tree definitions, or sums of marked up text snippets. */
  sealed trait Juxtaposable


  // No needed any more, TreeDef inherits directly from Juxtaposable
  // /** Tree definition that occurs in a juxtaposition of elements that
  //   * together form a forest.
  //   *
  //   * For example, the `/child()` tree in
  //   * {{{
  //   *   /parent(
  //   *     /child()
  //   *     "some text"
  //   *     /anotherChild()
  //   *   )
  //   * }}}
  //   * is a tree definition that appears as a part of juxtaposition.
  //   */
  // case class JuxtaposableTreeDef(treeDef: TreeDef) extends Juxtaposable


  /** Nonempty sum of text pieces. The text pieces can be marked up.
    *
    * For example,
    *
    * {{{
    * "foo bar" + "baz /embedded(tree) more text"
    * }}}
    *
    * is a `MarkupSum` with two summands.
    */
  case class MarkupSum(summands: List[Markup]) extends Juxtaposable


  /** Piece of text, possibly with embedded markup elements.
    *
    * Things that look like string literals (which could be annotated).
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
    *   unquotedIdentifier
    *   "some text without markup"
    *   "some text with %(12345) escapes %n"
    *   "some text /emph(with) markup"
    * }}}
    **/
  case class Markup(fragments: List[MarkupFragment])


  /** Everything that can appear inside a "string literal" with
    * marked up text.
    */
  sealed trait MarkupFragment


  /** Snippets of character data that are not interpreted in any way,
    * and are simply taken as-is directly from the input source code.
    *
    * Either parts of source code character data that does not contain
    * any escape sequences, or content of verbatim blocks.
    */
  case class SliceFragment(slices: List[StringSlice]) extends MarkupFragment


  /** Character data that has been interpreted in some way.
    *
    * E.g. a plus or a parenthesis that turned out to be ordinary text.
    */
  // case class InterpretedFragment(string: String) extends MarkupFragment

  /** Escaped unicode code point in hexadecimal notation.
    *
    * Passed as string with hexadecimal digits, not converted to integer yet.
    */
  case class UnicodeHexEscape(digits: StringSlice) extends MarkupFragment

  /** Escaped unicode code point in decimal notation.
    *
    * Passed as string with decimal digits, not converted to integer yet.
    */
  case class UnicodeDecimalEscape(digits: StringSlice) extends MarkupFragment

  /** Tree definition that appears in the middle of marked up text.
    *
    * Example:
    * {{{
    *   "text /tree() text"
    * }}}
    * The `/tree()` is an embedded tree.
    *
    */
  case class EmbeddedTree(treeDef: TreeDef) extends MarkupFragment

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
  case class TreeDef(labelParts: MarkupSum, children: Juxtaposition)
  extends Juxtaposable

}
