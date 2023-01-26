package ejoti.domain

import org.scalacheck.*
import org.scalacheck.Prop.*

//noinspection TypeAnnotation
object CollectedInfoChecks extends Properties("CollectedInfo") {

  val empty = CollectedInfo.empty

  property("Put a String and retrieve it") = forAll((s: String) => empty.add(s).access[String] == s)

  property("Put a Double and retrieve it") = forAll((d: Double) => empty.add(d).access[Double] == d)

  property("Put a Double and String and retrieve them") = forAll { (s: String, d: Double) =>
    val sd = empty.add(s).add(d)
    val ds = empty.add(d).add(s)

    Prop(sd.access[String] == s) &&
      Prop(sd.access[Double] == d) &&
      Prop(ds.access[String] == s) &&
      Prop(ds.access[Double] == d)
  }

  property("Adding is concatenating with singleton") = forAll { (s: String, d: Double) =>
    empty.add(s).add(d).info == (empty.add(s) ++ empty.add(d)).info
  }

  property("Adding a String is the same as adding the CollectedInfo with the String using the flatten concat") = forAll {
    (s: String, d: Double, b: Boolean) =>
      val base = empty + d + b
      try {
        Prop(base.add(s) == base.flattenedConcat(s)) && Prop(
          base.add(s) == base.flattenedConcat(empty + s)
        )
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          Prop.falsified
      }
  }

  property("++ is associative") = forAll {
    (s: String, d: Double, i: Int, b: Boolean, f: Float, c: Char) =>
      val one = empty + s + d
      val two = empty + i + b
      val three = empty + f + c

      ((one ++ two) ++ three) == (one ++ (two ++ three))
  }

  property("+ overrides") = forAll { (s: String, d1: Double, d2: Double) =>
    empty.add(s).add(d1).add(d2).access[Double] == d2
  }

  property("++ overrides") = forAll { (s: String, d1: Double, d2: Double, b: Boolean) =>
    empty.add(s).add(d1).concat(empty.add(d2).add(b)).access[Double] == d2
  }

  property("collected info to polymorphic function") = forAllNoShrink { (s: String, d: Double, b: Boolean, i: Int, f: Float, c: Char) =>

    try {
    val info = empty.add(s).add(d)
    val tup = (empty.add(i).add(b), f, c)
    val infoFunction = info.toPolymorphicFunction[tup.type]


    val result = infoFunction(tup)

    (Prop(result._1 == info.concat(empty.add(i).add(b))) && Prop(result._2 == info.add(f))) :| (
      s"""
      |Info: $info
      |Tup: $tup
      |Result 1: ${result._1}
      |Result 2: ${result._2}
      |""".stripMargin
    )
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        Prop.falsified
    }
    }

}
