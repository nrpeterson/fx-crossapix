package dev.nrp.puzzles.crossapix.heuristics

import dev.nrp.puzzles.crossapix.attributes.StateCounts
import dev.nrp.puzzles.crossapix.game.*

trait SaturatedLines

object SaturatedLines:
  given Heuristic3[SaturatedLines] with
    type Attr1 = Board
    type Attr2 = BoardState
    type Attr3 = StateCounts

    def getStep(board: Board, boardState: BoardState, stateCounts: StateCounts): Option[SolutionStep] =
      stateCounts.values
        .flatMap { case (lineId, sc) => List(RegionState.Black, RegionState.White).map { (lineId, sc, _) } }
        .collectFirst { case (lineId, sc, st) if sc.needed(st) > 0 && sc.needed(st) == sc.unknown =>
          val regions = board.lineView(lineId)
            .collect { case (regionId, _) if boardState.states.getOrElse(regionId, None).isEmpty => regionId }
            .toList

          val n = sc.unknown

          val d = desc"{l $lineId | Saturated Line} has $n unknown spaces, and needs $n $st spaces. So, all remaining "
            ++ desc"regions must be $st."

          SolutionStep(Explanation("Saturation", d), regions.map( Move(_, st) ))
        }
