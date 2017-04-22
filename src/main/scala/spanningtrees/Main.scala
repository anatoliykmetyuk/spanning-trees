package spanningtrees

import cats.instances.list._
import cats.syntax.show._

import all._
import typeclasses._

object Main {
  def main(args: Array[String]): Unit = {
    val n = 5

    val ls = allLetters[TreeLogger](n).written.map(Letter.make(n, _))
    val sorted = ls
      .groupBy(identity).map { case (k, v) => k -> v.size }
      .toList.sortBy(_._2).reverse.take(10)

    println(s"=== TOP 10 POPULAR LETTERS FOR $n ===\n")
    for { (l, freq) <- sorted }
      println(s"=== Frequency: $freq, Letter: $l ===\n${l.show}\n")
  }
}
