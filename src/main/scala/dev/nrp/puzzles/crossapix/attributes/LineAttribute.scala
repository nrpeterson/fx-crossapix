package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.game.{Board, LineId, Move}
import monocle.syntax.all.*
import monocle.Lens

/**
 * A specialized [[Attribute]] representing a characteristic computed line-by-line.
 *
 * Analogous to a `Map[LineId, P]`, though we don't use them directly as type inference can't work out the givens in
 * that case.  As such, you can generally think of type `A` as a wrapper around such a map.
 *
 * Note that we assume here that the `P` for a given [[dev.nrp.puzzles.crossapix.game.LineId]] will only be updated by
 * a [[dev.nrp.puzzles.crossapix.game.Move]] if that move impacts at least one space in the line.
 *
 * @tparam A The type of the attribute in question. Basically a wrapper around a `Map[LineId, P]`
 * @tparam P The type of the measurement computed for each [[dev.nrp.puzzles.crossapix.game.LineId]].
 */
trait LineAttribute[A, P] extends Attribute[A]:
  /** Method for building a wrapper `A` around a `Map[LineId, P]`. */
  def construct(values: Map[LineId, P]): A

  /** How to initialize a `P` for a given board and line. */
  def initializeLine(board: Board, lineId: LineId): P

  /** How to update the `P` for a given line that was impacted by the given [[dev.nrp.puzzles.crossapix.game.Move]] */
  def updateImpactedLine(board: Board, move: Move, lineId: LineId, curValue: P): P
  val map: Lens[A, Map[LineId, P]]

  /**
   * Initialize this [[Attribute]] given a [[dev.nrp.puzzles.crossapix.game.Board]].
   *
   * This is accomplished by calling [[LineAttribute.initializeLine]] for each line in the puzzle, forming a
   * map, and passing that map to [[LineAttribute.construct]].
   *
   * @param board The [[dev.nrp.puzzles.crossapix.game.Board]] to use for initialization.
   */
  override def initialize(board: Board): A =
    val values = board.lines
      .map { line => line->initializeLine(board, line) }
      .toMap
    
    construct(values)

  /**
   * Update this [[Attribute]] given its current value, the board, the current value, and a new move.
   *
   * Note that this update occurs by calling [[LineAttribute.updateImpactedLine]] on each line impacted by the given
   * move (that is, moves which involve at least one space in that line).
   *
   * @param board [[dev.nrp.puzzles.crossapix.game.Board]] (should match the [[dev.nrp.puzzles.crossapix.game.Board]]
   *              used to initialize)
   * @param move A new [[dev.nrp.puzzles.crossapix.game.Move]] to react to
   * @param curValue The value of this attribute before the current [[dev.nrp.puzzles.crossapix.game.Move]]
   *  @return Updated value of this [[Attribute]]
   */
  override def update(
    board: Board,
    move: Move,
    curValue: A
  ): A =
    val impactedLines = board.linesForRegion(move.regionId)

    impactedLines.foldLeft(curValue) { (cur, line) =>
      map.index(line).modify( p => updateImpactedLine(board, move, line, p) )(cur)
    }

object LineAttribute:
  def apply[A, P](using ev: LineAttribute[A, P]): LineAttribute[A, P] = ev