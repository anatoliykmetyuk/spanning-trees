package spanningtrees

import cats.Show

case class Letter(horizontalEdges: List[Boolean], verticalEdges: List[Boolean])

object Letter {
  def make(gridHeight: Int, decisions: List[Boolean]): Letter =
    Letter(decisions.take(gridHeight), decisions.drop(gridHeight))

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