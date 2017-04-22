package spanningtrees

import States._

object Trees extends Trees
trait Trees {
  type Tree[A] = List[A]

  def statesFor(x: Int): Tree[State] =
    if (x <= 1) List(List(0))
    else statesFor(x - 1).flatMap { state =>
      (newEqClass(state) :: state) :: collapse(state).map { _ :: state }
    }

  def booleanDecision(fate: Option[Boolean]): Tree[Boolean] = fate match {
    case Some(x) => List(x)
    case None    => List(true, false)
  }

  def horizontalTree(state: State, copied: Set[Int] = Set()): Tree[State] = state match {
    case Nil    => List(List())  // One choice - not to connect what we don't have
    case h :: t => booleanDecision( Some( !(copied(h) || t.contains(h)) ).filter(identity) ).flatMap {
      case true  => horizontalTree(t, copied + h).map { substate => h :: substate }
      case false => horizontalTree(t, copied    ).map { substate => newEqClass(state) :: substate }
    }
  }

  def verticalTree(state: State): Tree[State] =
    (0 until state.size - 1).foldLeft(List(state)) { (stateTree, id) =>
      for {
        st              <- stateTree
        (one, another)   = (st(id), st(id + 1))
        sameEqClassFate  = Some(one != another).filter(!_)
        connectThem     <- booleanDecision(sameEqClassFate)
      } yield connectThem match {
        case true =>
          val commonEqClass = math.min(one, another)
          st.map { case `one` | `another` => commonEqClass case x => x }
        
        case false => st
      }
    }

  def letters(state: State): Tree[State] = for {
    horizontalStep <- horizontalTree(state)
    verticalStep   <- verticalTree  (horizontalStep)
  } yield verticalStep

  def allLetters(m: Int): Tree[State] = for {
    state  <- statesFor(m)
    letter <- letters(state)
  } yield letter  
}
