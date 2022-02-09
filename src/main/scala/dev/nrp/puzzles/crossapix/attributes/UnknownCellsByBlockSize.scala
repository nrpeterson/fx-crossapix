package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.game.{Board, LineId, Move, RegionId}
import monocle.{Focus, Lens}
import monocle.syntax.all.*

/**
 * Recording of unknown regions that impact a given line, indexed by the size of their intersection with the line.
 * @param sizeToRegions Map from number of spaces to set of regions with that sized intersection.
 */
case class LineUnknownRegionsByBlockSize(sizeToRegions: Map[Int, List[RegionId]])

/** Wrapper around a `Map[LineId, LineUnknownRegionsByBlockSize]` representing the sizes of blocks in each line. */
case class UnknownRegionsByBlockSize(values: Map[LineId, LineUnknownRegionsByBlockSize])

object UnknownRegionsByBlockSize:
  /** Treat [[UnknownRegionsByBlockSize]] as a [[LineAttribute]]. */
  given regionsByBlockSize: LineAttribute[UnknownRegionsByBlockSize, LineUnknownRegionsByBlockSize] with
    val map: Lens[UnknownRegionsByBlockSize, Map[LineId, LineUnknownRegionsByBlockSize]] =
      Focus[UnknownRegionsByBlockSize](_.values)

    def construct(values: Map[LineId, LineUnknownRegionsByBlockSize]): UnknownRegionsByBlockSize =
      UnknownRegionsByBlockSize(values)

    def initializeLine(board: Board, lineId: LineId): LineUnknownRegionsByBlockSize =
      val sizeToRegions = board.lineView(lineId)
        .groupMap(_._2)(_._1)
        .map { case (size, regions) => (size, regions.toList) }

      LineUnknownRegionsByBlockSize(sizeToRegions)

    def updateImpactedLine(
      board: Board,
      move: Move,
      lineId: LineId,
      curValue: LineUnknownRegionsByBlockSize
    ): LineUnknownRegionsByBlockSize =
      val size = board.lineView(lineId)(move.regionId)
      curValue
        .focus(_.sizeToRegions.at(size))
        .modify {
          case None => None
          case Some(list) if list.contains(move.regionId) && list.length == 1 => None
          case Some(list) => Some(list.filterNot(_ == move.regionId))
        }
