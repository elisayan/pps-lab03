package it.unibo.pps.u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, filter}
import Sequence.*
import Person.*
import u03.Streams.Stream
import u03.{Sequences, Streams}
import u03.Streams.Stream.{Cons, Empty, cons, empty}

object Lab3 extends App {
  //Task 1

  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(_, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case _ => Nil()

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h1, t1) => Cons(h1, concat(t1, s2))
    case _ => s2

  def reverse[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))
    case _ => Nil()

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match {
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()
  }

  def distinct[A](s: Sequence[A]): Sequence[A] = s match {
    case Cons(h, t) => Cons(h, distinct(filter(t)(_ != h)))
    case Nil() => Nil()
  }


  //Task 2

  def getCourses(p: Sequence[Person]): Sequence[String] =
    map(filter(p) {
      case Teacher(_, _) => true
      case _ => false
    }) {
      case Teacher(_, course) => course
      case _ => ""
    }

  def foldLeft[A, B](l: Sequence[A])(n: B)(op: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t)(op(n, h))(op)
    case Nil() => n
  }

  def totalCourses(p: Sequence[Person]): Int =
    foldLeft(distinct(map(filter(p) {
      case Teacher(_, _) => true
      case _ => false
    }) {
      case Teacher(_, course) => course
      case _ => ""
    }))(0) {
      (acc, _) => acc + 1
    }

  //Task 3

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    import u03.Sequences.*
    import Sequence.*

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = s match
      case Cons(h, t) if p(h()) => cons(h(), takeWhile(t())(p))
      case _ => Empty()

    def fill[A](n: Int)(e: A): Stream[A] = n match
      case i if i > 0 => cons(e, fill(i - 1)(e))
      case _ => Empty()

    val fibonacci: Stream[Int] = Stream.map(Stream.iterate((0, 1))((a, b) => (b, a + b)))(_._1)

}