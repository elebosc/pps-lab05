package it.unibo.pps.task4

import it.unibo.pps.util.Sequences.Sequence
import Sequence.*
import it.unibo.pps.task1.Course

import scala.annotation.tailrec

object Task4:

  object sameCategory:
    def unapply(courses: Sequence[Course]): Option[String] =
      @tailrec
      def _getCategoryIfCommonToAllCourses(s: Sequence[Course])(prevCategory: Option[String]): Option[String] =
        (s, prevCategory) match
          case (Nil(), category) => category
          case (Cons(Course(_, _, _, category), _), Some(prevCategory)) if category != prevCategory => None
          case (Cons(Course(_, _, _, category), t), _) => _getCategoryIfCommonToAllCourses(t)(Some(category))
      _getCategoryIfCommonToAllCourses(courses)(None)

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
