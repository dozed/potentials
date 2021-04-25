import cats.kernel.{Eq, Order}
import cats.syntax.all._
import Util.{crossProduct, removeIndexed}

object Pots {

  type Domain = List[Int]
  type Assignment = List[Int]

  case class Variable(index: Int, name: String, domain: Domain)

  object Variable {
    implicit def varEqual: Eq[Variable] = Eq.fromUniversalEquals
    implicit def varOrder: Order[Variable] = Order.by(_.index)
  }

  case class Potential(variables: List[Variable], table: Map[Assignment, Double]) {
    def domain: List[Domain] = variables.map(_.domain)
    def assignments: List[Assignment] = crossProduct(domain)
  }

  object Potential {
    implicit def potEqual: Eq[Potential] = Eq.fromUniversalEquals
  }

  def disppot(pot: Potential): Unit = {
    val variables = pot.variables.map(v => {
      s"  ${v.name} (${v.index}): ${v.domain}"
    }).mkString("\n")

    val tab = pot.assignments.map(cfg => {
      f"  ${cfg.mkString(" ")} | ${evaluate(pot, cfg)}%.10f"
    }).mkString("\n")

    val txt = s"""Potential
                 |variables
                 |$variables
                 |table
                 |$tab
                 |--------------------------
     """.stripMargin

    println(txt)
  }

  // evaluate potential at one configuration
  def evaluate(potential: Potential, assignment: Assignment): Double = {
    potential.table(assignment)
  }

  // sum up potential at each assignment
  def totalpot(pot: Potential): Double = {
    pot.assignments.map(a => evaluate(pot, a)).sum
  }

  // multiply two potentials
  def multpots(pot1: Potential, pot2: Potential): Potential = {
    val newVariables = pot1.variables.concat(pot2.variables).ordDistinct
    val domains = newVariables.map(_.domain)
    val allAssignments = crossProduct(domains)

    val newTable = allAssignments.map(a => {
      val variableAssignmentMap = newVariables.map(_.index).zip(a).toMap

      val assignment1 = pot1.variables.map(v => variableAssignmentMap(v.index))
      val assignment2 = pot2.variables.map(v => variableAssignmentMap(v.index))

      val val1 = evaluate(pot1, assignment1)
      val val2 = evaluate(pot2, assignment2)

      a -> val1 * val2
    }).toMap

    Potential(newVariables, newTable)
  }

  def multpots(pot1: Potential, pot2: Potential, pots: Potential*): Potential = {
    (pot1 :: pot2 :: pots.toList).reduce(multpots)
  }

  // sum out one variable
  def sumpot(pot: Potential, v: Variable): Potential = {
    val idx = pot.variables.indexOf(v)
    if (idx === -1) sys.error(s"variable '$v' not found")

    val newVariables = removeIndexed(pot.variables, idx)

    val newTable = pot.assignments
      .groupBy(a => removeIndexed(a, idx))
      .view
      .mapValues(assignments => assignments.map(a => evaluate(pot, a)).sum)
      .toMap

    Potential(newVariables, newTable)
  }

  def sumpot(pot: Potential, vs: Variable*): Potential = {
    vs.toList.foldLeft(pot)((pot, v) => sumpot(pot, v))
  }

  // set variable to given state
  // - new potential does no longer contain the variable
  def setpot(pot: Potential, v: Variable, value: Int): Potential = {
    val idx = pot.variables.indexOf(v)
    if (idx === -1) sys.error(s"variable '$v' not found")

    val newVariables = removeIndexed(pot.variables, idx)

    val newTable =
      pot.table
        .view.filterKeys(a => a(idx) === value)
        .map({ case (a, p) => removeIndexed(a, idx) -> p })
        .toMap

    Potential(newVariables, newTable)
  }

  // conditional potential
  // - p(y1 y2 | x1 x2) = p(x1 x2 y1 y2) / p(x1 x2)
  def condpot(pot: Potential, condvars: List[Variable]): Potential = {
    val marVariables = pot.variables.diff(condvars)
    val marPot = sumpot(pot, marVariables:_*)

    val newTable = pot.assignments.map(a => {
      val variableAssignmentMap = pot.variables.zip(a).toMap
      val marAssignment = marPot.variables.map(v => variableAssignmentMap(v))

      val pxy = evaluate(pot, a)
      val px = evaluate(marPot, marAssignment)

      a -> (pxy / px)
    }).toMap

    Potential(pot.variables, newTable)
  }

}
