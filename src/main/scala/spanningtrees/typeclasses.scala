package spanningtrees

import cats.{Monad, MonoidK}
import cats.data.WriterT
import cats.instances.list._

object typeclasses {
  type TreeLogger[A] = WriterT[List, List[Boolean], A]

  implicit def treeLoggerMonad: Monad[TreeLogger] = new Monad[TreeLogger] {
    def pure[A](x: A): TreeLogger[A] = x match {
      case decision: Boolean => WriterT.put[List, List[Boolean], Boolean](decision)(List(decision)).asInstanceOf[TreeLogger[A]]
      case x => WriterT.put(x)(Nil)
    }

    def flatMap[A, B](fa: TreeLogger[A])(f: A => TreeLogger[B]): TreeLogger[B] =
      WriterT(for {
        w1                       <- fa.run
        (previousDecisions  , a)  = w1
        w2                       <- f(a).run
        (subsequentDecisions, b)  = w2
      } yield (previousDecisions ++ subsequentDecisions) -> b)

    def tailRecM[A, B](a: A)(f: A => TreeLogger[Either[A,B]]): TreeLogger[B] = ???    
  }

  implicit def treeLoggerMonoidK: MonoidK[TreeLogger] = new MonoidK[TreeLogger] {
    def empty[A]: TreeLogger[A] = WriterT[List, List[Boolean], A](Nil)
    def combineK[A](x: TreeLogger[A], y: TreeLogger[A]): TreeLogger[A] = WriterT(x.run ++ y.run)
  }
}