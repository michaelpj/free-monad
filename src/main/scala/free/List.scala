package free

import scalaz._

object List2 {
  case class Cons[+A, +B](head: A, tail: B)

  implicit def consFunctor[A]: Functor[({ type f[x] = Cons[A, x] })#f] =
    new Functor[({ type f[x] = Cons[A, x] })#f] {
      def map[B, C](a: Cons[A,B])(f: B => C) = a match {
        case Cons(head, tail) => Cons(head, f(tail))
      }
    }

  type List2[A] = Freee[({type f[+x] = Cons[A, x]})#f, Unit]
}


