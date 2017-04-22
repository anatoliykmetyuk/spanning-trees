package spanningtrees

object States extends States
trait States {
  /**
   * A state of a 1D slice of a spanning tree is represented by
   * the equivalence classes of its nodes.
   * An equivalence relationship is defined between two nodes if
   * they are connected either at this slice, or to the left of it.
   * This way, if no node is connected to another, the state
   * is List(0, 1, 2, 3) - every node has its own equivalence class.
   * If everyone is connected to everyone - the state is List(0, 0, 0, 0).
   */
  type State = List[Int]

  /**
   * All possible states in which a 1D slice of a given height can be.
   * Slices of like List(0, 1, 2, 1, 2) are invalid, since there is no
   * way for `1` and `2` to form separate equivalence classes.
   * However, a situation like List(0, 2, 1, 1, 2) is possible.
   */
  def statesFor(x: Int): List[State] =
    if (x <= 1) List(List(0))
    else statesFor(x - 1).flatMap { state =>
      (newEqClass(state) :: state) :: collapse(state).map { _ :: state }
    }

  def newEqClass(state: State): Int = state.max + 1

  /**
   * When trying to add another node to an existing 1D slice (effectively
   * growing the height of the slice), we are faced by the question of which
   * equivalence class we should assign it. We can assign it any class, provided
   * the nodes of this class are not surrounded by the members of another class.
   * If it is surrounded, there is no way to connect to the inner class without
   * connecting to the outer class too.
   *
   * This method eliminates the inner classes, leaving only the ones we can connect to. 
   */
  def collapse(lst: State): State = lst match {
    case lst if lst.distinct.size == lst.size => lst
    case h :: t if  t.contains(h) => h :: collapse( t.reverse.takeWhile(_ != h).reverse )
    case h :: t if !t.contains(h) => h :: collapse(t)
  }
}