package free

import scalaz._
import scalaz.Scalaz._
import scala.annotation.tailrec

sealed trait Freee[S[+_], +A]
case class Return[S[+_]: Functor, +A](run: A) extends Freee[S, A]
case class Roll[S[+_]: Functor, +A](run: ()=> Freee[S, A]) extends Freee[S, A] //needs to be ()=> so we can defer execution of mapping it

object Freee {
  // type-lambda mess to deal with the fact that we want a monad
  // in Freee's second type parameter
  implicit def FreeeMonad[S[+_]: Functor]: Monad[({ type f[x] = Freee[S, x] })#f] =
    new Monad[({ type f[x] = Freee[S, x] })#f] {
    
	  // just stick it in a Return
      def point[A](a: => A) = Return(a)
      
      override def map[A, B](a: Freee[S, A])(f: A => B): Freee[S, B] = a match {
        case Return(b) => Roll(()=> Return(f(b)))  //we need to roll it since we're combing two Freees into 'one;
        // we have to defer our recursion to the foldFreee so we don't blow the stack on our mapping
        case Roll(b) => Roll(() => b() match { //we do this to speed things up so we aren't double rolling always
          case Return(bb) => Return(f(bb))
          case Roll(bb) => bb().map(f)
        })
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
  
  @tailrec //let's make this tailrec so it's stack safe. 
  def foldFreee[S[+_]: Functor, A](a: Freee[S,A]): A = a match {
    case Return(b) => b
    case Roll(b) => b() match {
      case Return(bb: A) => bb
      case Roll(bb) => foldFreee(bb())
    }
  }
}
