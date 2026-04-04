package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.util.Optionals.Optional as ScalaOptional, ScalaOptional.*
import it.unibo.pps.util.Sequences.Sequence, Sequence.*
import it.unibo.pps.util.Streams, Streams.*

import scala.jdk.javaapi.OptionConverters
import scala.util.Random

trait ScalaLogics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class ScalaLogicsImpl(private val size: Int, private val minesToPlace: Int) extends ScalaLogics:

  private val random: Random = Random
  private val mines: Sequence[(Int, Int)] = generateMines()
  private var hitCells: Sequence[(Int, Int)] = Nil()

  private def randomIndex: Int = random.nextInt(size)

  private def generateMines(): Sequence[(Int, Int)] =
    var mines: Sequence[(Int, Int)] = Nil()
    while mines.length != minesToPlace do
      val position = (randomIndex, randomIndex)
      if !mines.contains(position) then mines = Cons(position, mines)
    mines

  private def getAdjacentMinesCount(x: Int, y: Int): Int =
    Stream.iterate(x - 1)(_ + 1).takeWhile(i => i <= x + 1)
      .flatMap(row => Stream
        .iterate(y - 1)(_ + 1).takeWhile(j => j <= y + 1)
        .map(col => (row, col))
      )
      .filter(position => mines.contains(position))
      .toList
      .length

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if mines.contains((x, y)) then
      OptionToOptional(Empty())
    else
      hitCells = Cons((x, y), hitCells)
      OptionToOptional(Just(getAdjacentMinesCount(x, y)))

  def won: Boolean = hitCells.length + mines.length == size * size
