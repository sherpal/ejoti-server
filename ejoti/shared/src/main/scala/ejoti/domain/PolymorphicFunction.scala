package ejoti.domain

/** A [[PolymorphicFunction]] is a component-wise function from a tuple X to a tuple Y.
  *
  * For a tuple input X = (X1, ..., XN) and tuple output type Y = (Y1, ..., YN), a [[PolymorphicFunction]] is
  * essentially a glorified tuple (X1 => Y1, ..., XN => YN)
  *
  * @param f
  *   underlying tuple of functions from types in X to the corresponding type in Y
  */
final class PolymorphicFunction[X <: Tuple, Y <: Tuple](f: PolymorphicFunction.UnderlyingPolymorphicFunction[X, Y])
    extends (X => Y) {

  /** Applies the component `Idx` function.
    *
    * Note that this function would not make sense for an arbitrary X => Y, but [[PolymorphicFunction]] live on a tiny
    * sub-space.
    */
  def applyIndex[Idx <: Int](x: Tuple.Elem[X, Idx])(using idxValue: ValueOf[Idx]): Tuple.Elem[Y, Idx] =
    f.toArray(idxValue.value).asInstanceOf[Tuple.Elem[X, Idx] => Tuple.Elem[Y, Idx]].apply(x)

  def apply(x: X): Y = {
    val xArray = x.toArray
    Tuple
      .fromArray(
        xArray.indices.toArray.map(idx => applyIndex[idx.type](xArray(idx).asInstanceOf[Tuple.Elem[X, idx.type]]))
      )
      .asInstanceOf[Y]
  }

  /** Sames as `applyIndex`, but using the embedded index from the [[Node.Choices]] instance.
    */
  def mapChoice(choice: Node.Choices[X]): Node.Choices[Y] = choice match {
    case Node.Value(value, idx) =>
      Node
        .Value(applyIndex[idx.type](value.asInstanceOf[Tuple.Elem[X, idx.type]]), idx)
        .asInstanceOf[Node.Choices[Y]]
  }

  /** Adds a new component in front of this [[PolymorphicFunction]].
    */
  def prepend[A, B](g: A => B): PolymorphicFunction[A *: X, B *: Y] = new PolymorphicFunction(g *: f)

}

object PolymorphicFunction {

  /** Type of Tuples accepted as input for a [[PolymorphicFunction]].
    */
  type UnderlyingPolymorphicFunction[X <: Tuple, Y <: Tuple] <: Tuple = (X, Y) match {
    case (EmptyTuple, EmptyTuple) => EmptyTuple
    case (x *: xtail, y *: ytail) => (x => y) *: UnderlyingPolymorphicFunction[xtail, ytail]
  }

  /** You can summon the [[PolymorphicFunction]] from an EmptyTuple to itself, since there is only one such function.
    */
  given empty: PolymorphicFunction[EmptyTuple, EmptyTuple] = new PolymorphicFunction(EmptyTuple)

  /** You can summon the identity [[PolymorphicFunction]].
    */
  given identityFunction[H, T <: Tuple](using PolymorphicFunction[T, T]): PolymorphicFunction[H *: T, H *: T] =
    summon[PolymorphicFunction[T, T]].prepend(identity[H])

  summon[PolymorphicFunction[String *: Int *: EmptyTuple, String *: Int *: EmptyTuple]]

  /** Lifts an implicit conversion between two heads at the [[PolymorphicFunction]] level.
    */
  given convertingHead[H1, H2, T1 <: Tuple, T2 <: Tuple](using
      headConversion: Conversion[H1, H2],
      tailFunction: PolymorphicFunction[T1, T2]
  ): PolymorphicFunction[H1 *: T1, H2 *: T2] =
    tailFunction.prepend(headConversion.apply)

  /** Gives you a [[PolymorphicFunction]] from H *: T1 to H *: T2 if you can provide a [[PolymorphicFunction]] from T1
    * to T2.
    */
  given mappingTail[H, T1 <: Tuple, T2 <: Tuple](using
      tailFunction: PolymorphicFunction[T1, T2]
  ): PolymorphicFunction[H *: T1, H *: T2] =
    tailFunction.prepend(identity[H])

}
