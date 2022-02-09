package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.game.{Board, LineId, Move, RegionId}
import monocle.{Focus, Lens}
import monocle.syntax.all.*

/** Map from lines to the collection of all regions that intersect them in an odd number of spaces.
 *
 * This powers heuristics such as [[dev.nrp.puzzles.crossapix.heuristics.Parity]] that look at the parity of a line
 * total as compared to the parity of the regions' intersections with the line.
 */
case class UnknownRegionsWithOddBlockSize(values: Map[LineId, Set[RegionId]])

object UnknownRegionsWithOddBlockSize:
  given oddBlockSizeAttribute: LineAttribute[UnknownRegionsWithOddBlockSize, Set[RegionId]] with
    val map: Lens[UnknownRegionsWithOddBlockSize, Map[LineId, Set[RegionId]]] = Focus[UnknownRegionsWithOddBlockSize](_.values)
    def construct(values: Map[LineId, Set[RegionId]]): UnknownRegionsWithOddBlockSize = UnknownRegionsWithOddBlockSize(values)

    def initializeLine(board: Board, lineId: LineId): Set[RegionId] =
      board.lineView(lineId)
        .collect { case (regionId, size) if size % 2 == 1 => regionId }
        .toSet

    def updateImpactedLine(
      board: Board,
      move: Move,
      lineId: LineId,
      curValue: Set[RegionId]
    ): Set[RegionId] =
      curValue.focus(_.at(move.regionId)).replace(false)