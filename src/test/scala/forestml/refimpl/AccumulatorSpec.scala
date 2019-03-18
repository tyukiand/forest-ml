package forestml.refimpl

import org.scalatest._

class AccumulatorSpec extends FlatSpec with Matchers {

  "AccumulatorBuilder" should "be usable by both producers and consumers" in {

    def produceInts[Acc <: Accumulator[Int, Acc]](acc: Acc): Acc = {
      acc
        .append(42)
        .append(58)
        .append(100)
    }
  
    val acc0 = AccumulatorBuilder.empty[Int]
    val resAcc = produceInts(acc0)
    val consumedList: List[Int] = resAcc.result
    consumedList should equal(List(42, 58, 100))
  }
}
