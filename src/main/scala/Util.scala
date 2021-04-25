import Pots.{Assignment, Domain}
import cats.syntax.all._

object Util {

  // remove elements from a list by indices
  def removeIndexed[A](xs: List[A], idxs: Set[Int]): List[A] = {
    xs.zipWithIndex.filterNot(a => idxs.contains(a._2)).map(_._1)
  }

  def removeIndexed[A](xs: List[A], idx: Int): List[A] = {
    removeIndexed(xs, Set(idx))
  }

  // D_1 x D_2 x ... x D_n
  def crossProduct(domains: List[Domain]): List[Assignment] = {
    for {
      permTail <- {
        if (domains.isEmpty) Nil
        else if (domains.size === 1) List(Nil)
        else crossProduct(domains.tail)
      }
      i <- domains.head
    } yield {
      i :: permTail
    }
  }


}
