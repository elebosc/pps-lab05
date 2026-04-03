package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private case class Pair[X, Y](x: X, y: Y)

  private val random: Random = Random
  private val initial: Pair[Int, Int] = Pair(randomPosition, randomPosition)
  private var tickCount: Int = 0

  private def randomPosition: Int = random.nextInt(size - 2) + 1

  override def tick(): Unit = tickCount += 1

  private def isOnVerticalAxis(x: Int, y: Int): Boolean =
    x == initial.x && Math.abs(y - initial.y) <= tickCount

  private def isOnHorizontalAxis(x: Int, y: Int): Boolean =
    y == initial.y && Math.abs(x - initial.x) <= tickCount

  private def isOnForwardDiagonal(x: Int, y: Int): Boolean =
    x - y == initial.x - initial.y && Math.abs(x - initial.x) <= tickCount

  private def isOnBackwardDiagonal(x: Int, y: Int): Boolean =
    x + y == initial.x + initial.y && Math.abs(x - initial.x) <= tickCount

  override def hasElement(x: Int, y: Int): Boolean =
    isOnVerticalAxis(x, y) || isOnHorizontalAxis(x, y) || isOnForwardDiagonal(x, y) || isOnBackwardDiagonal(x, y)

  private def isOutOfBound(p: Int) = p - tickCount < 0 || p + tickCount >= size

  override def isOver: Boolean = isOutOfBound(initial.x) || isOutOfBound(initial.y)
