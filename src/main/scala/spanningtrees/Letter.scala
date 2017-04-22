package spanningtrees

import cats.Show

case class Letter(horizontalEdges: List[Boolean], verticalEdges: List[Boolean])

object Letter {
  def make(gridHeight: Int, decisions: List[Boolean]): Letter =
    Letter(decisions.take(gridHeight), decisions.drop(gridHeight))

  /**
   * In presence of `import cats.syntax.show._`, this
   * allows to use `letter.show` syntax to convert letters to
   * Strings.
   *
   * Note that this type class is in the companion object of Letter.
   * This way, there is no need to import it explicitly, since the
   * compiler searches the companions for type classes.
   */
  implicit def letterShow: Show[Letter] = new Show[Letter] {
    def show(l: Letter): String = {
      def row(h: Boolean) =  (if (h) "-" else " ") * 4 + "X"
      def col(v: Boolean) = (" " * 4 + (if (v) "|" else " ") + "\n") * 4
      
      (l.horizontalEdges.dropRight(1).zip(l.verticalEdges)
         .map { case (h, v) => row(h) + "\n" + col(v) }
      :+ row(l.horizontalEdges.last)).mkString
    }
  }
}