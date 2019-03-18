package forestml.refimpl

sealed trait Value
case class Tree(name: String, children: Vector[Value]) extends Value
case class Text(s: String) extends Value