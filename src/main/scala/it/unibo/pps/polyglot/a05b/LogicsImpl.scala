package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val random: Random = Random
  private val (xi, yi) = (randomIndex, randomIndex)
  private var tickCount: Int = 0

  private def randomIndex: Int = random.nextInt(size - 2) + 1

  override def tick(): Unit = tickCount += 1

  private def isWithinCurrentRange(delta: Int): Boolean = Math.abs(delta) <= tickCount

  private def isOnCurrentVerticalAxis(x: Int, y: Int): Boolean =
    val isOnVerticalAxis = x == xi
    isOnVerticalAxis && isWithinCurrentRange(y - yi)

  private def isOnCurrentHorizontalAxis(x: Int, y: Int): Boolean =
    val isOnHorizontalAxis = y == yi
    isOnHorizontalAxis && isWithinCurrentRange(x - xi)

  private def isOnCurrentForwardDiagonal(x: Int, y: Int): Boolean =
    val isOnForwardDiagonal = x - y == xi - yi
    isOnForwardDiagonal && isWithinCurrentRange(x - xi)

  private def isOnCurrentBackwardDiagonal(x: Int, y: Int): Boolean =
    val isOnBackwardDiagonal = x + y == xi + yi
    isOnBackwardDiagonal && isWithinCurrentRange(x - xi)

  override def hasElement(x: Int, y: Int): Boolean =
    isOnCurrentVerticalAxis(x, y) || isOnCurrentHorizontalAxis(x, y) ||
    isOnCurrentForwardDiagonal(x, y) || isOnCurrentBackwardDiagonal(x, y)

  private def isOutOfBoundsGivenCenterIndex(centerIndex: Int): Boolean = 
    centerIndex - tickCount < 0 || centerIndex + tickCount >= size

  override def isOver: Boolean = isOutOfBoundsGivenCenterIndex(xi) || isOutOfBoundsGivenCenterIndex(yi)
