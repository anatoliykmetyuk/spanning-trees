package spanningtrees

import cats.instances.list._  // To be able to call `written` below, which takes some List type class which Cats has.
import cats.syntax.show._     // To be able to call `l.show` on the letters. This import injects the method `show` in the letter via the rich wrapper pattern.

import all._          // Trees with States API
import typeclasses._  // TreeLogger type to log the decisions (which edges to make) made during tree construction, and the type classes for it.

/**
 * Finds all transition letters for all the states of a grid
 * of a given height.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val n = 5
    println(s"Number of nodes in one column: $n. Possible states: ${statesFor(n).size}.")

    val ls = allLetters[TreeLogger](n).written.map(Letter.make(n, _))
    val sorted = ls
      .groupBy(identity).map { case (k, v) => k -> v.size }
      .toList.sortBy(_._2).reverse.take(10)

    println(s"=== TOP 10 (out of ${ls.size} overall, ${ls.distinct.size} distinct) POPULAR LETTERS FOR $n ===\n")
    for { (l, freq) <- sorted }
      println(s"=== Frequency: $freq, Letter: $l ===\n${l.show}\n")
  }
}
