package u04lab
import u03.Sequences.*
import Sequence.*
import u03.extensionmethods.Optionals.*
import Optional.*

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  trait Traversable[T[_]]:
    extension [A](t: T[A]) def traverse(f: A => Unit): Unit

  object TraversableSequence extends Traversable[Sequence]:
    extension [A](s: Sequence[A]) @tailrec override def traverse(f: A => Unit): Unit = s match
      case Cons(h, t) => f(h); t.traverse(f)
      case _ => ()

  object TraversableOptional extends Traversable[Optional]:
    extension [A](o: Optional[A]) override def traverse(f: A => Unit): Unit = o match
      case Just(a) => f(a)
      case _ => ()

import Ex5Traversable.log
@main def main(): Unit =
  {
    import Ex5Traversable.TraversableSequence.*
    println("Testing Traversable[Sequence]")
    val s = Cons(1, Cons(2, Nil()))
    s.traverse(log)
    s.traverse(println)
  }
  {
    import Ex5Traversable.TraversableOptional.*
    println("Testing Traversable[Optional]")
    val o = Just(1)
    o.traverse(log)
    o.traverse(println)
  }
