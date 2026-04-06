package it.unibo.pps.task4

import it.unibo.pps.util.Sequences.Sequence, Sequence.*
import it.unibo.pps.task1.Course

object Task4:

  object sameCategory:
    def unapply(courses: Sequence[Course]): Option[String] = courses match
      case Nil() => None
      case Cons(h, _) => if courses.all(c => c.category == h.category) then Some(h.category) else None

  def logCategoryComparison(courses: Sequence[Course]): Unit =
    courses match
      case sameCategory(cat) => println(s"Courses have same category $cat")
      case _ => println("Courses have no common category")

@main def trySameCategoryExtractor(): Unit =

  import Task4.*

  // Test with courses of same category
  val course1 = Course("PY1", "Introduction to Python", "Mario Rossi", "Programming")
  val course2 = Course("C1", "Introduction to C", "Mario Verdi", "Programming")
  val course3 = Course("JS1", "Introduction to JavaScript", "Mario Bianchi", "Programming")
  var courses = Sequence(course1, course2, course3)
  logCategoryComparison(courses)

  // Test with courses of different categories
  val course4 = Course("CSEC1", "Fundamentals of Cybersecurity", "Mario Rossi", "Cybersecurity")
  courses = Sequence(course1, course2, course4)
  logCategoryComparison(courses)

  // Test on empty courses list
  courses = Nil()
  logCategoryComparison(courses)
