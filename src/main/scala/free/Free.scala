package free

import scalaz._
import scalaz.Scalaz._

sealed trait Freee[S[+_], +A]
case class Return[S[+_]: Functor, +A](run: A) extends Freee[S, A]
case class Roll[S[+_]: Functor, +A](run: S[Freee[S, A]]) extends Freee[S, A]

object Freee {
  // type-lambda mess to deal with the fact that we want a monad
  // in Freee's second type parameter
  implicit def FreeeMonad[S[+_]: Functor]: Monad[({ type f[x] = Freee[S, x] })#f] =
    new Monad[({ type f[x] = Freee[S, x] })#f] {
    
	  // just stick it in a Return
      def point[A](a: => A) = Return(a)
      
      override def map[A, B](a: Freee[S, A])(f: A => B): Freee[S, B] = a match {
        case Return(b) => Return(f(b))
        // use the implicit functor we have for S to map over the outer S, then
        // recursively call ourselves on the inner Freee
        case Roll(b) => Roll(b.map(map(_)(f)))
      }

      override def join[A](a: Freee[S, Freee[S, A]]): Freee[S, A] = a match {
        // the outer Freee can be either a Return or a Roll
        // b has type Freee[S, A]
        case Return(b) => b
        // b has type S[Freee[S, Freee[S,A]]], so we use the implicit functor
        // to map over the S, and then use our own map to call flatten recursively
        // inside
        case Roll(b) => Roll(b.map(join(_)))
      }
      
      // scalaz wants this for a Monad instance
      def bind[A, B](fa: Freee[S, A])(f: A => Freee[S, B]): Freee[S, B] = join(map(fa)(f))
    }
  
  def liftFreee[S[+_]: Functor, A](a: S[A]): Freee[S, A] = 
    Roll(a.map(Return(_)))
  
  def foldFreee[S[+_]: Functor, A](a: Freee[S,A])(f: S[A] => A): A = a match {
    case Return(b) => b
    case Roll(b) => f(b.map(foldFreee(_)(f)))
  }
}
