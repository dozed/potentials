import Pots._
import cats.syntax.all._

object DemoAppleTree extends App {

  val yes = 1
  val no = 0

  val isApple = Variable(0, "isApple", List(yes, no))
  val isFruit = Variable(1, "isFruit", List(yes, no))
  val growsOnTree = Variable(2, "growsOnTree", List(yes, no))

  // p(isFruit|isApple)
  val allApplesAreFruitsPot = Potential(
    List(isFruit, isApple),
    Map(
      List(yes, yes) -> 1.0,
      List(no, yes) -> 0.0,
      List(yes, no) -> 0.5,
      List(no, no) -> 0.5,
    )
  )

  // p(growsOnTree|isFruit)
  val allFruitsGrowOnTreesPot = Potential(
    List(growsOnTree, isFruit),
    Map(
      List(yes, yes) -> 1.0,
      List(no, yes) -> 0.0,
      List(yes, no) -> 0.5,
      List(no, no) -> 0.5,
    )
  )

  val jointPot = multpots(allApplesAreFruitsPot, allFruitsGrowOnTreesPot)

  disppot(jointPot)

  // all apples grow on trees
  println("p(growsOnTree|isApple=yes)")
  disppot((sumpot(isFruit) >>> setpot(isApple, yes))(jointPot))

}
