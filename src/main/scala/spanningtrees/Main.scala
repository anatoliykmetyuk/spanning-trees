package spanningtrees


object Main {
  type Tree[A] = List[A]
  type State   = List[Int]

  def collapse(lst: List[Int]): List[Int] = lst match {
    case lst if lst.distinct.size == lst.size => lst
    case h :: t if  t.contains(h) => h :: collapse( t.reverse.takeWhile(_ != h).reverse )
    case h :: t if !t.contains(h) => h :: collapse(t)
  }

  def statesFor(x: Int): Tree[State] =
    if (x == 1) List(List(0))
    else statesFor(x - 1).flatMap { state =>
      ((state.max + 1) :: state) :: collapse(state).map { _ :: state }
    }

  def main(args: Array[String]): Unit = {
    println(statesFor(5).size)
  }
}
