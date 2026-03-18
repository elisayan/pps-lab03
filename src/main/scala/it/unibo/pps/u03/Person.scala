package it.unibo.pps.u03

// An ADT: type + module
enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Person:
  def name(p: Person): String = p match
    case Student(n, _) => n
    case Teacher(n, _) => n

import Person.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

// a method outside the Person module
def isStudent(p: Person): Boolean = p match
  case Student(_, _) => true
  case _ => false

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

@main def testPerson() =
  import Person.*
  println(Person.name(Student("mario", 2015)))
  println(name(Student("mario", 2015)))
  println(isStudent(Student("mario", 2015)))

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(foldLeft(lst)(0)(_ - _)) // -16

  val l1 =
    Cons(Teacher("Viroli", "PPS"),
      Cons(Teacher("Aguzzi", "PPS"),
        Cons(Teacher("Ricci", "PCD"),
          Nil())))
  println(totalCourses(l1)) // 2
