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
  
  private def isWithinCurrentRange(delta: Int): Boolean = Math.abs(delta) <= tickCount

  private def isOnCurrentVerticalAxis(x: Int, y: Int): Boolean =
    val isOnVerticalAxis = x == initial.x
    isOnVerticalAxis && isWithinCurrentRange(y - initial.y)

  private def isOnCurrentHorizontalAxis(x: Int, y: Int): Boolean =
    val isOnHorizontalAxis = y == initial.y
    isOnHorizontalAxis && isWithinCurrentRange(x - initial.x)

  private def isOnCurrentForwardDiagonal(x: Int, y: Int): Boolean =
    val isOnForwardDiagonal = x - y == initial.x - initial.y
    isOnForwardDiagonal && isWithinCurrentRange(x - initial.x)

  private def isOnCurrentBackwardDiagonal(x: Int, y: Int): Boolean =
    val isOnBackwardDiagonal = x + y == initial.x + initial.y
    isOnBackwardDiagonal && isWithinCurrentRange(x - initial.x)

  override def hasElement(x: Int, y: Int): Boolean =
    isOnCurrentVerticalAxis(x, y) || isOnCurrentHorizontalAxis(x, y) || 
    isOnCurrentForwardDiagonal(x, y) || isOnCurrentBackwardDiagonal(x, y)

  private def isOutOfBound(p: Int): Boolean = p - tickCount < 0 || p + tickCount >= size

  override def isOver: Boolean = isOutOfBound(initial.x) || isOutOfBound(initial.y)
