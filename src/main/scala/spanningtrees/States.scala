package spanningtrees

object States extends States
trait States {
  type State = List[Int]

  def collapse(lst: State): State = lst match {
    case lst if lst.distinct.size == lst.size => lst
    case h :: t if  t.contains(h) => h :: collapse( t.reverse.takeWhile(_ != h).reverse )
    case h :: t if !t.contains(h) => h :: collapse(t)
  }

  def newEqClass(state: State): Int = state.max + 1

  def statesFor(x: Int): List[State] =
    if (x <= 1) List(List(0))
    else statesFor(x - 1).flatMap { state =>
      (newEqClass(state) :: state) :: collapse(state).map { _ :: state }
    }
}