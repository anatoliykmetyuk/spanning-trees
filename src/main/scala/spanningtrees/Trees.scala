package spanningtrees

import cats.{Monad, SemigroupK, MonoidK}
import cats.syntax.all._

import States._

/**
 * Everything is a tree.
 *
 * A letter is a sequence of two major decisions:
 * - Which horizontal edges to make?
 * - Which vertical edges to make?
 *
 * These two, in turn, are composed of minor, binary decisions:
 * whether to make a particular edge or not. So, the entire letter
 * can be described by the path we took through the decision binary
 * tree while constructing all the edges of the letter.
 *
 * The below code describes what it means to make a decision on every level.
 *
 * `Tree` is a Monad. A Monad abstracts a particular effect, so that
 * we do not need to load our brains with it. Normally, the effect
 * of a "Tree" is multiplicity of choice: a single level of a tree would
 * be nicely describable by a collection, so our effect would be a `List[_]`.
 *
 * This indeed is possible, but we also will want another effect: memory.
 * Not only we want to describe that we have multiple choices, we also want
 * to remember the path leading to each choice.
 *
 * This is why, below a `Tree` is not a concrete type, but a generic one.
 * We only know that it is a monad - meaning we can flatMap (hence compose) it,
 * and also we have a Monoid for it - meaning we can compose two trees. 
 */
object Trees extends Trees
trait Trees {
  /** When we have a binary choice. If `fate` is defined - we have only one choice. */
  def booleanDecision[Tree[_]](fate: Option[Boolean])(implicit M: Monad[Tree], S: SemigroupK[Tree]): Tree[Boolean] = fate match {
    case Some(x) => M.pure(x)  // A single `Boolean` choice is wrapped into the `Tree` effect (becomes `Tree[Boolean]`). What is this effect? This method does not need to care: its job is to specify the choices, not the effects.
    case None    => S.algebra[Boolean].combine(M.pure(true), M.pure(false))  // We need to combine two choices into one decision tree if fate is not defined.
  }

  /**
   * Which horizontal edges can we construct? Whichever we want, but one constraint:
   * no node should be left behind, disconnected from the tree, or else this
   * will no longer be a tree.
   *
   * So this method works recursively, one element at a time. Every node's
   * horizontal edge is decided via `booleanDecision`. If we see that this is
   * the last element of a given equivalence class and it was not yet
   * connected (`copied`) to the next slice, we don't have other choice but to
   * connect it.
   */
  def horizontalTree[Tree[_]: SemigroupK](state: State, copied: Set[Int] = Set())(implicit M: Monad[Tree]): Tree[State] = state match {
    case Nil    => M.pure(List())  // One choice - not to connect what we don't have
    case h :: t => booleanDecision( Some( !(copied(h) || t.contains(h)) ).filter(identity) ).flatMap {  // Fancy way to define the `fate`: if the node is the last of its equivalence class, the fate will be `Some(true)` (connect); else - `None`.
      case true  => horizontalTree(t, copied + h).map { substate => h :: substate }
      case false => horizontalTree(t, copied    ).map { substate => newEqClass(state) :: substate }
    }
  }

  /**
   * Which vertical edges can we construct?
   * The only constraint is that we can't connect two nodes of the same equivalence
   * class. This method works similarly to the previous one.
   */
  def verticalTree[Tree[_]: SemigroupK](state: State)(implicit M: Monad[Tree]): Tree[State] =
    (0 until state.size - 1).foldLeft(M.pure(state)) { (stateTree, id) =>
      for {  // Monadic flow - without it, we would have ended up with nested `flatMap` and `map`, which is spaghetti.
        st              <- stateTree
        (one, another)   = (st(id), st(id + 1))
        sameEqClassFate  = Some(one != another).filter(!_)
        connectThem     <- booleanDecision(sameEqClassFate)
      } yield connectThem match {
        case true =>  // If we connect two nodes of different equivalence classes, we need to decide on the new common class and update the state so that other nodes of these classes also get a new class.
          val commonEqClass = math.min(one, another)
          st.map { case `one` | `another` => commonEqClass case x => x }
        
        case false => st
      }
    }

  /**
   * This is where the "glue" nature of a Monad is best visible.
   * A letter is composed of a sequential application of the horizontal edges
   * followed by the vertical edges. We don't know the effect that happens while
   * we make these edges (the `Tree` effect), but it is enough to know that
   * we can compose trees together with `flatMap`.
   */
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
