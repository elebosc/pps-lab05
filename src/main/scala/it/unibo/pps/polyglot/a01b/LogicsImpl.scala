package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.util.Optionals.Optional as ScalaOptional, ScalaOptional.*
import it.unibo.pps.util.Sequences.Sequence, Sequence.{empty => _, flatMap => _, *}
import it.unibo.pps.util.Set, Set.*
import it.unibo.pps.util.Streams, Streams.*

import scala.jdk.javaapi.OptionConverters
import scala.util.Random

trait ScalaLogics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class ScalaLogicsImpl(private val size: Int, private val minesToPlace: Int) extends ScalaLogics:

  private val random: Random = Random
  private val mines: Set[(Int, Int)] = generateMines()
  private var hitCells: Set[(Int, Int)] = empty

  private def randomIndex: Int = random.nextInt(size)

  private def generateMines(): Set[(Int, Int)] =
    var mines: Set[(Int, Int)] = empty
    while mines.toSequence.length != minesToPlace do
      val position = (randomIndex, randomIndex)
      mines = union(fromElement(position), mines)
    mines

  private def getAdjacentMinesCount(x: Int, y: Int): Int =
    Stream.rangeClosed(x - 1, x + 1)
      .flatMap(row => Stream.rangeClosed(y - 1, y + 1)
        .map(col => (row, col))
      )
      .filter(position => mines.contains(position))
      .toList
      .length

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if mines.contains((x, y)) then
      OptionToOptional(Empty())
    else
      hitCells = union(fromElement(x, y), hitCells)
      OptionToOptional(Just(getAdjacentMinesCount(x, y)))

  def won: Boolean = hitCells.toSequence.length + mines.toSequence.length == size * size
