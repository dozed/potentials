import Pots._
import Util._
import cats.syntax.all._
import munit.FunSuite

class Tests extends FunSuite {

  test("removeIndexed") {

    val xs = List(0, 1, 2, 3, 4, 5)

    assertEquals(List(0, 2, 3, 5), removeIndexed(xs, Set(1, 4)))

  }

  test("crossProduct") {

    val domains = List(List(0, 1), List(0, 1, 2))
    val expected = List(List(0, 0), List(1, 0), List(0, 1), List(1, 1), List(0, 2), List(1, 2))

    assertEquals(expected, crossProduct(domains))

  }

  test("sumpot on empty variables is the original potential") {

    import DemoBurglar.radioPot

    assert(sumpot(radioPot) === radioPot)

  }


}
