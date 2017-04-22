package spanningtrees

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen.choose

import all._

object MainSpec extends Properties("MainSpec") {

  property("States of letters tree of the state with only one horizontal choice == states after vertical tree") =
    forAll(choose(1, 5)) { n: Int => letters((0 to n).toList) == verticalTree((0 to n).toList) }

}