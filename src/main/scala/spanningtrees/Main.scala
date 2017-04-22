package spanningtrees

object Main {
  type Tree[A] = List[A]

  type State = List[Int]

  def collapse(lst: State): State = lst match {
    case lst if lst.distinct.size == lst.size => lst
    case h :: t if  t.contains(h) => h :: collapse( t.reverse.takeWhile(_ != h).reverse )
    case h :: t if !t.contains(h) => h :: collapse(t)
  }

  def newEqClass(state: State): Int = state.max + 1

  def statesFor(x: Int): Tree[State] =
    if (x == 1) List(List(0))
    else statesFor(x - 1).flatMap { state =>
      (newEqClass(state) :: state) :: collapse(state).map { _ :: state }
    }

  def booleanDecision(fate: Option[Boolean]): Tree[Boolean] = fate match {
    case Some(x) => List(x)
    case None    => List(true, false)
  }

  def horizontalTree(state: State, copied: Set[Int]): Tree[State] = state match {
    case Nil    => List(List())  // One choice - not to connect what we don't have
    case h :: t => booleanDecision( Some( !(copied(h) || t.contains(h)) ).filter(identity) ).flatMap {
      case true  => horizontalTree(t, copied + h).map { substate => h :: substate }
      case false => horizontalTree(t, copied    ).map { substate => newEqClass(state) :: substate }
    }
  }

  def main(args: Array[String]): Unit = {
    println( horizontalTree(List(0, 0, 0), Set()) )
  }
}

object StateDefs {

}
