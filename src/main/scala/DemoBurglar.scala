import Pots._

object DemoBurglar extends App {

  // states
  val yes = 1
  val no = 0

  // variables
  val burglar = Variable(0, "burglar", List(yes, no))
  val earthquake = Variable(1, "earthquake", List(yes, no))
  val alarm = Variable(2, "alarm", List(yes, no))
  val radio = Variable(3, "radio", List(yes, no))

  // potentials
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

  disppot(jointPot)

  // p(burglar|alarm=yes)
  disppot(setpot(condpot(sumpot(jointPot, earthquake, radio), List(alarm)), alarm, yes))

  // p(burglar|alarm=yes, radio=yes)
  disppot(setpot(setpot(condpot(sumpot(jointPot, earthquake), List(alarm, radio)), alarm, yes), radio, yes))

}
