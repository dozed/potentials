import Pots._
import cats.syntax.all._

object DemoStudent extends App {

  // states
  val easy = 0
  val difficult = 1

  val notIntelligent = 0
  val intelligent = 1

  val gradeA = 0
  val gradeB = 1
  val gradeC = 2

  val low = 0
  val high = 1

  val weak = 0
  val strong = 1

  // variables
  val difficulty = Variable(0, "difficulty", List(easy, difficult))
  val intelligence = Variable(1, "intelligence", List(notIntelligent, intelligent))
  val grade = Variable(2, "grade", List(gradeA, gradeB, gradeC))
  val sat = Variable(3, "sat", List(low, high))
  val letter = Variable(4, "letter", List(weak, strong))

  // potentials
  val difficultyPot = Potential(
    List(difficulty),
    Map(
      List(easy) -> 0.6,
      List(difficult) -> 0.4,
    )
  )

  val intelligencePot = Potential(
    List(intelligence),
    Map(
      List(notIntelligent) -> 0.7,
      List(intelligent) -> 0.3,
    )
  )

  val gradePot = Potential(
    List(grade, intelligence, difficulty),
    Map(
      List(gradeA, notIntelligent, easy) -> 0.3,
      List(gradeB, notIntelligent, easy) -> 0.4,
      List(gradeC, notIntelligent, easy) -> 0.3,
      List(gradeA, notIntelligent, difficult) -> 0.05,
      List(gradeB, notIntelligent, difficult) -> 0.25,
      List(gradeC, notIntelligent, difficult) -> 0.7,
      List(gradeA, intelligent, easy) -> 0.9,
      List(gradeB, intelligent, easy) -> 0.08,
      List(gradeC, intelligent, easy) -> 0.02,
      List(gradeA, intelligent, difficult) -> 0.5,
      List(gradeB, intelligent, difficult) -> 0.3,
      List(gradeC, intelligent, difficult) -> 0.2,
    )
  )

  val satPot = Potential(
    List(sat, intelligence),
    Map(
      List(low, notIntelligent) -> 0.95,
      List(high, notIntelligent) -> 0.05,
      List(low, intelligent) -> 0.2,
      List(high, intelligent) -> 0.8,
    )
  )

  val letterPot = Potential(
    List(grade, letter),
    Map(
      List(gradeA, weak) -> 0.1,
      List(gradeA, strong) -> 0.9,
      List(gradeB, weak) -> 0.4,
      List(gradeB, strong) -> 0.6,
      List(gradeC, weak) -> 0.99,
      List(gradeC, strong) -> 0.01,
    )
  )


  val jointPot = multpots(difficultyPot, intelligencePot, gradePot, satPot, letterPot)

  disppot(jointPot)

  println(totalpot(jointPot))

  println("p(letter)")
  disppot(sumpot(difficulty, intelligence, grade, sat)(jointPot))

  println("p(letter|intelligence=notIntelligent)")
  disppot((sumpot(difficulty, grade, sat) >>> condpot(intelligence) >>> setpot(intelligence, notIntelligent))(jointPot))

  println("p(letter|intelligence=notIntelligent, difficulty=easy)")
  disppot((sumpot(grade, sat) >>> condpot(intelligence, difficulty) >>> setpot(intelligence, notIntelligent) >>> setpot(difficulty, easy))(jointPot))

  println("p(intelligence|grade=gradeC)")
  disppot((sumpot(difficulty, letter, sat) >>> condpot(grade) >>> setpot(grade, gradeC))(jointPot))

  println("p(difficulty|grade=gradeC)")
  disppot((sumpot(intelligence, letter, sat) >>> condpot(grade) >>> setpot(grade, gradeC))(jointPot))

  println("p(intelligence|letter=weak)")
  disppot((sumpot(difficulty, grade, sat) >>> condpot(letter) >>> setpot(letter, weak))(jointPot))

  println("p(intelligence|grade=gradeC, letter=weak)")
  disppot((sumpot(difficulty, sat) >>> condpot(letter, grade) >>> setpot(grade, gradeC) >>> setpot(letter, weak))(jointPot))

  println("p(intelligence|grade=gradeC, sat=high)")
  disppot((sumpot(difficulty, letter) >>> condpot(sat, grade) >>> setpot(grade, gradeC) >>> setpot(sat, high))(jointPot))

  println("p(difficulty|grade=gradeC, sat=high)")
  disppot((sumpot(intelligence, letter) >>> condpot(sat, grade) >>> setpot(grade, gradeC) >>> setpot(sat, high))(jointPot))

  println("p(intelligence|grade=gradeC, difficulty=difficult)")
  disppot((sumpot(sat, letter) >>> condpot(difficulty, grade) >>> setpot(grade, gradeC) >>> setpot(difficulty, difficult))(jointPot))

  println("p(intelligence|grade=gradeB)")
  disppot((sumpot(difficulty, sat, letter) >>> condpot(grade) >>> setpot(grade, gradeB))(jointPot))

  println("p(intelligence|grade=gradeB, difficulty=difficult)")
  disppot((sumpot(sat, letter) >>> condpot(difficulty, grade) >>> setpot(grade, gradeB) >>> setpot(difficulty, difficult))(jointPot))

}
