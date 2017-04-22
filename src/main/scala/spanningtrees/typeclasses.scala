package spanningtrees

import cats.{Monad, MonoidK}
import cats.data.WriterT
import cats.instances.list._

/**
 * In `Trees.scala`, we emphasized several times that
 * the methods in that class focus only on the tree construction
 * logic, not on the effects this process can trigger.
 *
 * Here is where we specify the effects we want. And we want, first,
 * to highlight the choice nature of the state continuation
 * (from one state, we can go to many states, and we often work).
 * Second, we want to remember the decisions we made while constructing
 * the states (by traversing the binary choice trees). We want these
 * decisions because we will be able to construct and visualize "letters"
 * via them.
 */
object typeclasses {
  /**
   * The effect type we will use.
   *
   * Employs two techniques:
   * - Writer monad
   * - Monad transformers
   *
   * A Writer monad is simply a logger. Defined as a pair
   * `(L, V)`, where `L` is a log and `V` is a value,
   * a `Writer[V]` means that we have a value `V` with some log `L`.
   * We can write to the log `L` at will for whatever purposes.
   *
   * A monad transformers is a way to stack two monads together.
   * Why stack monads? Monads very often represent effects that
   * happen during computation (because pure FP implies your function can't have
   * side effects, the side effects go to the return types).
   * Very often we have more than one effect. Like here:
   * we want both to reflect the choice (a `List` effect) and
   * the memory (a `Writer` effect).
   *
   * Signatures of monads ending with `T` are extremely common.
   * First type argument usually means the type of another effect you
   * want to stack with this one, and `A` means the actual result type
   * you are computing (in the case of Trees.scala, we compute State more
   * often than not).
   *
   * The type below essentially translates to `List[Writer[List[Boolean], A]]` -
   * which essentially translates to `List[(List[Boolean], A)]`. If you look at its
   * scaladoc, you'll see that it is a `case class WriterT[F[_], L, V](run: F[(L, V)])`.
   *
   * So, many pairs of values `A` and logs of boolean choices we made to arrive to `A`!
   */
  type TreeLogger[A] = WriterT[List, List[Boolean], A]

  /**
   * We need this to specify how to compose TreeLoggers with flatMap (which we use
   * extensively in Trees.scala), but also how to create new instances of
   * TreeLogger.
   */
  implicit def treeLoggerMonad: Monad[TreeLogger] = new Monad[TreeLogger] {
    /**
     * Creates a new instance of TreeLogger.
     *
     * `WriterT.put(x)(y)` creates a writer with log `x` and value `y`.
     * We want to log binary choices we make. So, whenever we call `pure(Boolean)`, we
     * log that Boolean. Other values go along unlogged.
     */
    def pure[A](x: A): TreeLogger[A] = x match {
      case decision: Boolean => WriterT.put[List, List[Boolean], Boolean](decision)(List(decision)).asInstanceOf[TreeLogger[A]]
      case x => WriterT.put(x)(Nil)
    }

    /**
     * We want to compose one set of pairs: (log of decisions, state) with another one,
     * computed from the first one via flatMap on states.
     *
     * To do that, we need to compute the new pairs (decisions, states) and append
     * the current decisions to the computed decisions.
     */
    def flatMap[A, B](fa: TreeLogger[A])(f: A => TreeLogger[B]): TreeLogger[B] =
      WriterT(for {
        w1                       <- fa.run
        (previousDecisions  , a)  = w1
        w2                       <- f(a).run
        (subsequentDecisions, b)  = w2
      } yield (previousDecisions ++ subsequentDecisions) -> b)

    /**
     * Tail recursive application of flatMap, in a loop. Required by the Monad
     * definition, but is often left undefined.
     */
    def tailRecM[A, B](a: A)(f: A => TreeLogger[Either[A,B]]): TreeLogger[B] = ???    
  }

  /**
   * This is to compose two TreeLoggers without flatMap.
   * The difference is that we use flatMap to continue the computation on
   * the value under `TreeLogger` effect (in which case we need to
   * compose the lists of decisions made). We use `combine` when we have two
   * independent values under TreeLogger effect (no need to combine the decisions).
   */
  implicit def treeLoggerMonoidK: MonoidK[TreeLogger] = new MonoidK[TreeLogger] {
    def empty[A]: TreeLogger[A] = WriterT[List, List[Boolean], A](Nil)
    def combineK[A](x: TreeLogger[A], y: TreeLogger[A]): TreeLogger[A] = WriterT(x.run ++ y.run)
  }
}