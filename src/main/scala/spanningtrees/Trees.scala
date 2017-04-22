package spanningtrees

import cats.{Monad, SemigroupK, MonoidK}
import cats.syntax.all._

import States._

object Trees extends Trees
trait Trees {
  def booleanDecision[Tree[_]](fate: Option[Boolean])(implicit M: Monad[Tree], S: SemigroupK[Tree]): Tree[Boolean] = fate match {
    case Some(x) => M.pure(x)
    case None    => S.algebra[Boolean].combine(M.pure(true), M.pure(false))
  }

  def horizontalTree[Tree[_]: SemigroupK](state: State, copied: Set[Int] = Set())(implicit M: Monad[Tree]): Tree[State] = state match {
    case Nil    => M.pure(List())  // One choice - not to connect what we don't have
    case h :: t => booleanDecision( Some( !(copied(h) || t.contains(h)) ).filter(identity) ).flatMap {
      case true  => horizontalTree(t, copied + h).map { substate => h :: substate }
      case false => horizontalTree(t, copied    ).map { substate => newEqClass(state) :: substate }
    }
  }

  def verticalTree[Tree[_]: SemigroupK](state: State)(implicit M: Monad[Tree]): Tree[State] =
    (0 until state.size - 1).foldLeft(M.pure(state)) { (stateTree, id) =>
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

  def letters[Tree[_]: Monad: SemigroupK](state: State): Tree[State] = for {
    horizontalStep <- horizontalTree(state)
    verticalStep   <- verticalTree  (horizontalStep)
  } yield verticalStep

  def allLetters[Tree[_]](m: Int)(implicit M: Monad[Tree], S: MonoidK[Tree]): Tree[State] = {
    import cats.instances.list._
    for {
      state  <- statesFor(m).map(M.pure).combineAll(S.algebra[State])
      letter <- letters[Tree](state)
    } yield letter
  }
}
