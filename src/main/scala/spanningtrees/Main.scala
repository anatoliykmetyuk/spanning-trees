package spanningtrees

object Trees extends Trees
trait Trees {
  type Tree[A] = List[A]

  type State = List[Int]

  def collapse(lst: State): State = lst match {
    case lst if lst.distinct.size == lst.size => lst
    case h :: t if  t.contains(h) => h :: collapse( t.reverse.takeWhile(_ != h).reverse )
    case h :: t if !t.contains(h) => h :: collapse(t)
  }

  def newEqClass(state: State): Int = state.max + 1

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

object Main extends Trees {
  def main(args: Array[String]): Unit = {
    println( statesFor(2) == allLetters(2 - 1) )
    println(statesFor(2))
    println
    println(allLetters(1))
  }
}
