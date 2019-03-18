/*
object SumGrammarExample {

  object SumGrammar extends Grammar[Char] {
  
    sealed trait Ast
    case class Num(i: Int) extends Ast
    case class Sum(summands: List[Ast]) extends Ast
  
    lazy val atom: L[Ast] = delay { 
      (('1': L[String]) | '2' | '3') ^ { (x: String) => Num(x.toInt) } |
      ('(' ~> sum <~ ')')
    }
  
    lazy val sum: L[Ast] = delay { 
      atom ~ ('+' ~> atom).* ^ { case (x: Ast, xs: List[Ast]) => Sum(x :: xs) }
    }
  
  }

  def main(args: Array[String]): Unit = {  
    import SumGrammar._
    println(isNullable(sum))
    println(isNullable(atom))
    println(s"FIRST(atom) = " + firstSet(atom))
    println(s"FIRST(sum) = " + firstSet(sum))
  }
}

object NullableGrammarExample {
  object FunnyGrammar extends Grammar[Char] {
    lazy val a: L[Any] = delay { (a ~ b) | c }
    lazy val b: L[Any] = Empty | 'b'
    lazy val c: L[Any] = Empty | 'c'
    lazy val d: L[Any] = c ~ 'd'
    lazy val eps: L[Any] = Empty
  }

  def main(args: Array[String]): Unit = {  
    import FunnyGrammar._
    println(isNullable(a) + " should be true")
    println(isNullable(b) + " should be true")
    println(isNullable(c) + " should be true")
    println(isNullable(d) + " should be false")
    println(isNullable(eps) + "should be true")

    println(s"FIRST(a) = " + firstSet(a) + " (should be {c, b})")
    println(s"FIRST(b) = " + firstSet(b) + " (should be {b})")
    println(s"FIRST(c) = " + firstSet(c) + " (should be {c})")
    println(s"FIRST(d) = " + firstSet(d) + " (should be {c, d})")
  }
}
*/