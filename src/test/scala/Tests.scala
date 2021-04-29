import Pots._
import Util._
import cats.syntax.all._
import munit.FunSuite

class Tests extends FunSuite {

  test("removeIndexed") {

    val xs = List(0, 1, 2, 3, 4, 5)

    assertEquals(removeIndexed(xs, Set(1, 4)), List(0, 2, 3, 5))

  }

  test("crossProduct") {

    val domains = List(List(0, 1), List(0, 1, 2))
    val expected = List(List(0, 0), List(1, 0), List(0, 1), List(1, 1), List(0, 2), List(1, 2))

    assertEquals(crossProduct(domains), expected)

  }

  object DemoBurglar {

    val yes = 1
    val no = 0

    val burglar = Variable(0, "burglar", List(yes, no))
    val earthquake = Variable(1, "earthquake", List(yes, no))
    val alarm = Variable(2, "alarm", List(yes, no))
    val radio = Variable(3, "radio", List(yes, no))

    val burglarPot = Potential(
      List(burglar),
      Map(
        List(yes) -> 0.01,
        List(no) -> 0.99,
      )
    )

    val earthquakePot = Potential(
      List(earthquake),
      Map(
        List(yes) -> 0.000001,
        List(no) -> (1.0 - 0.000001),
      )
    )

    val radioPot = Potential(
      List(radio, earthquake),
      Map(
        List(yes, yes) -> 1.0,
        List(no, yes) -> 0.0,
        List(yes, no) -> 0.0,
        List(no, no) -> 1.0,
      )
    )

    val alarmPot = Potential(
      List(alarm, burglar, earthquake),
      Map(
        List(yes, yes, yes) -> 0.9999,
        List(yes, yes, no) -> 0.99,
        List(yes, no, yes) -> 0.99,
        List(yes, no, no) -> 0.0001,
        List(no, yes, yes) -> (1 - 0.9999),
        List(no, yes, no) -> (1 - 0.99),
        List(no, no, yes) -> (1 - 0.99),
        List(no, no, no) -> (1 - 0.0001),
      )
    )

    val jointPot = multpots(burglarPot, earthquakePot, radioPot, alarmPot)

  }

  import DemoBurglar._

  test("sumpot on empty variables is the original potential") {

    assertEquals(sumpot()(radioPot), radioPot)

  }

  test("potential sums up to 1.0") {

    assertEqualsDouble(totalpot(jointPot), 1.0, 0.00001)
    assertEqualsDouble(totalpot(sumpot(earthquake, radio, alarm)(jointPot)), 1.0, 0.00001)
    assertEqualsDouble(totalpot((sumpot(earthquake, burglar) >>> condpot(radio) >>> setpot(radio, yes))(jointPot)), 1.0, 0.00001)

  }

  test("marginal is the original potential") {

    assertEquals(burglarPot, sumpot(earthquake, radio, alarm)(jointPot))

  }


}
