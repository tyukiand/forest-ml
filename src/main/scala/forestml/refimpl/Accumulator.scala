package forestml.refimpl

/** Interface that is facing the producers of `X`s that 
  * we want to accumulate.
  */
trait Accumulator[X, +Acc <: Accumulator[X, Acc]] {

  /** Appends the element `x` and returns a new accumulator. */
  def append(x: X): Acc
}

/** Interface that is exposed to the consumers of the results
  * produced by an accumulator.
  */
trait Builder[Result] {

  /** Returns the result with the accumulated elements. */
  def result: Result
}

/** Combines the interfaces [[Accumulator]] and [[Builder]] into one.
  *
  * The intended use is as follows: the consumer of a `Result` is supposed
  * to pass an `AccumulatorBuilder[X, Result]` to a producer of `X`, get
  * a modified `AccumulatorBuilder[X, Result]` back, and invoke `result` on 
  * it. In this way, the consumer does not have to know anything about the
  * concrete type of the `Accumulator[X, Acc]` that it receives.
  */
trait AccumulatorBuilder[X, Res]
extends Accumulator[X, AccumulatorBuilder[X, Res]] with Builder[Res]

// AccumulatorBuilderLike[X, Res, AccumulatorBuilder[X, Res]]

/** Entry point for working with [[Accumulator]]s and [[Builder]]s.
  *
  * Provides the method `empty` that produces an empty [[AccumulatorBuilder]].
  */
object AccumulatorBuilder {

  /** Creates an empty accumulator. */
  def empty[X]: AccumulatorBuilder[X, List[X]] = ListAccumulator[X](Nil)
}

/** Immutable accumulator that prepends elements to a reversed list.
  * 
  * When `result` is called, brings the list into the correct order and
  * returns it.
  */
case class ListAccumulator[X](reversedList: List[X])
extends AccumulatorBuilder[X, List[X]] 
   with Accumulator[X, ListAccumulator[X]] {

  def append(x: X) = ListAccumulator(x :: reversedList)
  def result: List[X] = reversedList.reverse
}

