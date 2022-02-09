package dev.nrp.puzzles.crossapix.attributes
import dev.nrp.puzzles.crossapix.game.{Board, LineId, Move, RegionState}
import monocle.{Focus, Lens}
import monocle.syntax.all.*

/**
 * Basic counts of spaces in different states within a given line.
 * @param total Total spaces (in any state) in the line
 * @param blackNeeded Number of currently unknown spaces that must be marked black
 * @param whiteNeeded Number of currently unknown spaces that must be marked white
 * @param unknown Total number of currently unknown spaces
 * @param black Total number of spaces currently marked black
 * @param white Total number of spaces currently marked white
 */
case class LineStateCounts(
  total: Int,
  blackNeeded: Int,
  whiteNeeded: Int,
  unknown: Int,
  black: Int,
  white: Int
):
  /** Convenience method for accessing either [[blackNeeded]] or [[whiteNeeded]] based on a region state. */
  def needed(state: RegionState): Int = state match
    case RegionState.Black => blackNeeded
    case RegionState.White => whiteNeeded

/** Wrapper around a `Map[LineId, LineStateCounts]` for use as a [[LineAttribute]]. */
case class StateCounts(values: Map[LineId, LineStateCounts])

object StateCounts:
  /** [[LineAttribute]] for representing single line state counts as an [[Attribute]]. */
  given LineAttribute[StateCounts, LineStateCounts] with
    /** Lens for zooming in to the wrapped `Map[LineId, LineStateCounts]` inside a [[StateCounts]] */
    val map: Lens[StateCounts, Map[LineId, LineStateCounts]] = Focus[StateCounts](_.values)

    /** Wrap a `Map[LineId, LineStateCounts]` with a `StateCounts` instance */
    def construct(values: Map[LineId, LineStateCounts]): StateCounts = StateCounts(values)

    /** Compute basic stats from the given line on a brand new board. */
    def initializeLine(board: Board, lineId: LineId): LineStateCounts =
      val (total, blackNeeded) = lineId match
        case LineId.Row(i) => (board.numCols, board.rowTotals(i))
        case LineId.Col(j) => (board.numRows, board.colTotals(j))

      LineStateCounts(total, blackNeeded, total - blackNeeded, total, 0, 0)

    /** Update basic count stats for a line impacted by a new move. */
    def updateImpactedLine(
      board: Board,
      move: Move,
      lineId: LineId,
      curValue: LineStateCounts
    ): LineStateCounts =
      val numSpaces = board.lineView(lineId)(move.regionId)

      val result: LineStateCounts = move.state match
        case RegionState.Black =>
          curValue
            .focus(_.unknown).modify(_ - numSpaces)
            .focus(_.blackNeeded).modify(_ - numSpaces)
            .focus(_.black).modify(_ + numSpaces)

        case RegionState.White =>
          curValue
            .focus(_.unknown).modify(_ - numSpaces)
            .focus(_.whiteNeeded).modify(_ - numSpaces)
            .focus(_.white).modify(_ + numSpaces)
      
      result