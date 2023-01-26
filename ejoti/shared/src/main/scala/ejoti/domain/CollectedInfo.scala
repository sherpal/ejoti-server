package ejoti.domain

import scala.annotation.targetName
import scala.compiletime.ops.int.*

/** An instance of a [[CollectedInfo]] is to be seen as a map from the types composing the Info tuple to an instance of
  * that type.
  *
  * [[CollectedInfo]] have methods to be composed and add other elements. Since it is supposed to be a Map, adding
  * elements from of existing types will erase the previous ones.
  *
  * In order to make them practical, there are a lot of implicit conversions wherever possible.
  *
  * @example
  *
  * {{{
  *   val info = CollectedInfo.empty + 3 + "hello" + true
  *   info.access[Int] // returns 3
  *   info.access[String] // returns "hello"
  *   info.access[Boolean] // returns true
  *   info.access[Double] // does not compile
  * }}}
  *
  * @param info
  */
final class CollectedInfo[Info <: Tuple] private (val info: Info) {
  private lazy val infoArray = info.toArray

  /** Returns the instance of type PieceOfInfo stored in this [[CollectedInfo]]. Does not compile if the type is not
    * contained.
    */
  def access[PieceOfInfo](using indexValue: ValueOf[CollectedInfo.IndexOf[PieceOfInfo, Info]]): PieceOfInfo =
    infoArray(indexValue.value).asInstanceOf[PieceOfInfo]

  /** Returns a [[CollectedInfo]] by only keeping the types in the SubsetInfo tuple.
    */
  def subset[SubsetInfo <: Tuple](using
      ValueOf[CollectedInfo.IndexesOf[SubsetInfo, Info]],
      CollectedInfo.Elems[Info, CollectedInfo.IndexesOf[SubsetInfo, Info]] =:= SubsetInfo
  ): CollectedInfo[SubsetInfo] = CollectedInfo(CollectedInfo.transform(info))

  /** Adds or override the new type with the provided instance.
    */
  @targetName("addInfo")
  def +[NewPieceOfInfo](pieceOfInfo: NewPieceOfInfo): CollectedInfo[NewPieceOfInfo *: Info] =
    CollectedInfo[NewPieceOfInfo *: Info](pieceOfInfo *: info)

  /** Alias for + */
  def add[NewPieceOfInfo](pieceOfInfo: NewPieceOfInfo): CollectedInfo[NewPieceOfInfo *: Info] = this + pieceOfInfo

  /** Adds all info from that [[CollectedInfo]] to this one. Existing types will be overriden.
    */
  @targetName("concatInfo")
  def ++[OtherInfo <: Tuple](that: CollectedInfo[OtherInfo]): CollectedInfo[Tuple.Concat[OtherInfo, Info]] =
    CollectedInfo(that.info ++ this.info)

  /** Alias for ++ */
  def concat[OtherInfo <: Tuple](that: CollectedInfo[OtherInfo]): CollectedInfo[Tuple.Concat[OtherInfo, Info]] =
    this ++ that

  /** Concatenates if the NewInfo is also a [[CollectedInfo]], otherwise adds the NewInfo.
    */
  def flattenedConcat[NewInfo](newInfo: NewInfo): CollectedInfo[CollectedInfo.FlattenedConcat[Info, NewInfo]] =
    (newInfo match {
      case t: CollectedInfo[u] => this ++ t
      case _                   => this + newInfo
    }).asInstanceOf[CollectedInfo[CollectedInfo.FlattenedConcat[Info, NewInfo]]]

  /** Creates a [[PolymorphicFunction]] returning a tuple with Tuple.Size[X] copy of this [[CollectedInfo]], where each
    * component is augmented with the type at the corresponding entry in X.
    */
  def toPolymorphicFunction[X <: Tuple](using
      xLength: ValueOf[Tuple.Size[X]]
  ): PolymorphicFunction[X, CollectedInfo.MappedCollectedInfo[X, Info]] =
    CollectedInfo.addCollectedInfo(this)

  override def toString: String = s"CollectedInfo(${infoArray.mkString(", ")})"

  override def equals(obj: Any): Boolean = obj match {
    case that: CollectedInfo[_] => this.info == that.info
    case other                  => false
  }

  override def hashCode(): Int = info.hashCode()

}

object CollectedInfo {

  type Empty = CollectedInfo[EmptyTuple]
  def empty: Empty = CollectedInfo(EmptyTuple)

  type FlattenedConcat[Info <: Tuple, NewInfo] <: Tuple = NewInfo match {
    case CollectedInfo[t] => Tuple.Concat[t, Info]
    case _                => NewInfo *: Info
  }
  type LiftedToCollectedInfo[NewInfo] = FlattenedConcat[EmptyTuple, NewInfo]

  def liftToCollectedInfo[NewInfo](newInfo: NewInfo): CollectedInfo[LiftedToCollectedInfo[NewInfo]] =
    empty.flattenedConcat(newInfo)

  private object internal {
    type IndexOfAcc[X, T <: Tuple, CurrentIndex <: Int] <: Int = (T, CurrentIndex) match {
      case (X *: _, _)  => CurrentIndex
      case (_ *: xs, _) => IndexOfAcc[X, xs, CurrentIndex + 1]
    }
  }
  type IndexOf[X, T <: Tuple] = internal.IndexOfAcc[X, T, 0]

  /** Returns whether the type A is in Tuple Y. */
  type Contains[A, Y <: Tuple] <: Boolean = Y match {
    case EmptyTuple => false
    case A *: tail  => true
    case _ *: tail  => Contains[A, tail]
  }
  type NotContains[A, Y <: Tuple] <: Boolean = Contains[A, Y] match {
    case true  => false
    case false => true
  }

  /** Returns a Tuple containing types of X that are *not* in Y. */
  type ElemsNotIn[X <: Tuple, Y <: Tuple] = Tuple.Filter[X, [x] =>> NotContains[x, Y]]

  private val info1 = empty.+("coucou").+(3.2)
  private val info2 = empty.+(3).+(true).+("hello")

  private val info = info1 ++ info2

  assert(info1.access[String] == "coucou")
  assert(info1.access[Double] == 3.2)
  assert(info2.access[Int]    == 3)
  assert(info2.access[Boolean])
  assert(info2.access[String]            == "hello")
  assert(info.access[String]             == "hello") // 2 overrides 1
  assert((info2 ++ info1).access[String] == "coucou") // 1 overrides 2
  // info2.access[Double] // does not compile

  type Elems[X <: Tuple, Indexes <: Tuple] <: Tuple = Indexes match {
    case EmptyTuple      => EmptyTuple
    case 0 *: indexes    => Tuple.Elem[X, 0] *: Elems[X, indexes]
    case S[n] *: indexes => Tuple.Elem[X, n + 1] *: Elems[X, indexes]
  }

  summon[Elems[String *: Int *: Double *: EmptyTuple, 1 *: 0 *: EmptyTuple] =:= (Int *: String *: EmptyTuple)]

  def applyIndexes[X <: Tuple, Indexes <: Tuple](
      x: X,
      indexes: Indexes
  ): Elems[X, Indexes] = {
    val xArray = x.toArray
    runtime.Tuples
      .fromArray(indexes.toArray.map(idx => xArray(idx.asInstanceOf[Int])))
      .asInstanceOf[Elems[X, Indexes]]
  }

  /** Represents the indexes of types in X in the tuple Y */
  type IndexesOf[X <: Tuple, Y <: Tuple] = Tuple.Map[X, [x] =>> IndexOf[x, Y]]

  given ValueOf[EmptyTuple] = ValueOf(EmptyTuple)

  given [X, XS <: Tuple](using ValueOf[X], ValueOf[XS]): ValueOf[X *: XS] =
    ValueOf(summon[ValueOf[X]].value *: summon[ValueOf[XS]].value)

  def transform[X <: Tuple, Y <: Tuple](
      x: X
  )(using indexes: ValueOf[IndexesOf[Y, X]], ev: Elems[X, IndexesOf[Y, X]] =:= Y): Y =
    ev(applyIndexes(x, indexes.value))

  summon[IndexesOf[String *: EmptyTuple, Int *: String *: Double *: EmptyTuple] =:= (1 *: EmptyTuple)]

  given [X <: Tuple, Y <: Tuple](using
      indexes: ValueOf[IndexesOf[Y, X]],
      ev: Elems[X, IndexesOf[Y, X]] =:= Y
  ): Conversion[CollectedInfo[X], CollectedInfo[Y]] = (x: CollectedInfo[X]) => CollectedInfo(transform(x.info))

  given unitTupleIsEssentiallyEmpty[T <: Tuple]: Conversion[CollectedInfo[T], CollectedInfo[Unit *: EmptyTuple]] =
    _ => CollectedInfo.empty + ()

  import scala.compiletime.erasedValue

  type MappedCollectedInfo[X <: Tuple, Info <: Tuple] = Tuple.Map[X, [x] =>> CollectedInfo[FlattenedConcat[Info, x]]]

  summon[
    MappedCollectedInfo[String *: CollectedInfo[
      Int *: String *: EmptyTuple
    ] *: EmptyTuple, Double *: Boolean *: EmptyTuple] =:= (
      CollectedInfo[(String *: Double *: Boolean *: EmptyTuple)] *: CollectedInfo[
        (Int *: String *: Double *: Boolean *: EmptyTuple)
      ] *: EmptyTuple
    )
  ]

  def addCollectedInfo[X <: Tuple, Info <: Tuple](
      collectedInfo: CollectedInfo[Info]
  )(using xLength: ValueOf[Tuple.Size[X]]): PolymorphicFunction[X, MappedCollectedInfo[X, Info]] =
    new PolymorphicFunction(
      Tuple
        .fromArray(Array.fill(xLength.value)(collectedInfo.flattenedConcat))
        .asInstanceOf[PolymorphicFunction.UnderlyingPolymorphicFunction[X, MappedCollectedInfo[X, Info]]]
    )

}
