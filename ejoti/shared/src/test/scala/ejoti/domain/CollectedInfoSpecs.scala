package ejoti.domain

import CollectedInfo.given

final class CollectedInfoSpecs extends munit.FunSuite {

  test("Checking that these type compile") {
    summon[ValueOf[CollectedInfo.IndexesOf[String *: String *: EmptyTuple, String *: EmptyTuple]]]
  }

}
