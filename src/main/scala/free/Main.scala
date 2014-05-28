package free

import scalaz._
import scalaz.Scalaz._
import scalaz.Kleisli._

object Main {
  def main(args: Array[String]) = {
    free
  }
  
  def naive = {
    val foo: Reader[Int, Seq[Int]] = for {
      x <- (1 to 3000).toList.map(_ => Reader[Int, Int](identity)).sequenceU
    } yield x

    println(foo(1).sum)
  }
  
  // type alias for Free partially applied to Id
  type FreeId[+A] = Freee[Id, A]
  // type alias for ReaderT on top of FreeId
  type FreeReader[A, B] = ReaderT[FreeId, A, B]
  object FreeReader {
    // convenience method for making FreeReaders, analagous to the apply method on
    // Reader. The kleisli is due to the fact that Readers are implemented as 
    // Kleislis (functions A => M[B]) in scalaz
    def apply[A, B](f: A => B) : FreeReader[A, B] = kleisli(f(_).point[FreeId])
  }
  
  def free = {
    val foo: FreeReader[Int, Seq[Int]] = for {
      x <- (1 to 3000).toList.map(i => FreeReader[Int, Int](ii => 1)).sequenceU
    } yield x
    
    // after running our reader we need to get out of Free, which we do by using 
    // "go", which is foldFree. Getting out of our suspension functor is trivial
    // (it's Id!), so we formally use the copoint method
    println(Freee.foldFreee(foo(1)).sum)
  }
}
