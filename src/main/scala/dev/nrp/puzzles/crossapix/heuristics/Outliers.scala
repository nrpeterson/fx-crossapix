package dev.nrp.puzzles.crossapix.heuristics

import dev.nrp.puzzles.crossapix.attributes.{StateCounts, UnknownRegionsByBlockSize}
import dev.nrp.puzzles.crossapix.game.*
import dev.nrp.puzzles.crossapix.utils.MapUtils

trait Outliers

object Outliers:
  given Heuristic2[Outliers] with
    type Attr1 = UnknownRegionsByBlockSize
    type Attr2 = StateCounts
    def getStep(unknownBlocks: UnknownRegionsByBlockSize, stateCounts: StateCounts): Option[SolutionStep] =
      val lineProps = MapUtils.zip(stateCounts.values, unknownBlocks.values)

      lineProps.toList
        .flatMap { case (lineId, (sc, ub)) => List((lineId, sc, ub, RegionState.Black), (lineId, sc, ub, RegionState.White)) }
        .collectFirst { case (lineId, sc, ub, st) if ub.sizeToRegions.exists { case (size, _) => size > sc.needed(st) } =>

          val regions = ub
            .sizeToRegions
            .filter { case (size, _) => size > sc.needed(st) }
            .flatMap { case (_, regions) => regions }
            .toList
            .sorted

          val n = sc.needed(st)
          val op = RegionState.opposite(st)
          val description =
            desc"{l $lineId | Line with outliers} can only have $n more $st spaces. So, any region which intersects "
              ++ desc"this line in more than $n spaces must be $op."
          SolutionStep(Explanation("Outlier", description), regions.map { Move (_, RegionState.opposite (st)) } )
        }






