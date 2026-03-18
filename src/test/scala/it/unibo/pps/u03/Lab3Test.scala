package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.*
import Sequence.*
import u03.Streams.Stream
import u03.Optionals.*
import Optional.*
import it.unibo.pps.u03.Person.{Student, Teacher}

class Lab3Test extends LambdaTest {

  //Task 1
  val s = Cons(10, Cons(20, Cons(30, Nil())))

  @Test
  def testSkip(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), skip(s)(1))
    assertEquals(Cons(30, Nil()), skip(s)(2))
    assertEquals(Nil(), skip(s)(3))

  @Test
  def testZip(): Unit =
    val s2 = Cons("a", Cons("b", Cons("c", Nil())))
    assertEquals(Cons((10, "a"), Cons((20, "b"), Cons((30, "c"), Nil()))), zip(s, s2))
    assertEquals(Nil(), zip(s, Nil()))

  @Test
  def testConcat(): Unit =
    val s2 = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), concat(s, s2))
    assertEquals(s, concat(Nil(), s))

  @Test
  def testReverse(): Unit =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(s))
    assertEquals(Nil(), reverse(Nil()))

  @Test
  def testFlatMap(): Unit =
    val result = flatMap(s)(n => Cons(n, Cons(n + 1, Nil())))
    val expected = Cons(10, Cons(11, Cons(20, Cons(21, Cons(30, Cons(31, Nil()))))))
    assertEquals(expected, result)

  @Test
  def testDistinct(): Unit =
    val sDup = Cons(1, Cons(2, Cons(1, Cons(3, Cons(2, Nil())))))
    assertEquals(Cons(1, Cons(2, Cons(3, Nil()))), distinct(sDup))
    assertEquals(Nil(), distinct(Nil()))

  //Task 2
  @Test
  def testGetCourses(): Unit =
    val p = Cons(Student("m", 2015), Cons(Teacher("c", "pps"), Nil()))
    assertEquals(Cons("pps", Nil()), getCourses(p))

  @Test
  def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test
  def testTotalCourses(): Unit =
    val lst = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Aguzzi", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil())))
    assertEquals(2, totalCourses(lst))

  //Task 3

  @Test
  def testTakeWhile(): Unit=
    val s = Stream.iterate(0)(_ + 1)
    val result = Stream.toList(Stream.takeWhile(s)(_ < 5))
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), result)
  
  @Test
  def testFill(): Unit=
    val result = Stream.toList(Stream.fill(3)("a"))
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), result)
}
