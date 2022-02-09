package dev.nrp.puzzles.crossapix.heuristics

import dev.nrp.puzzles.crossapix.attributes.{StateCounts, UnknownRegionsWithOddBlockSize}
import dev.nrp.puzzles.crossapix.game.*

trait Parity

object Parity:
  given Heuristic2[Parity] with
    type Attr1 = UnknownRegionsWithOddBlockSize
    type Attr2 = StateCounts

    def getStep(
      oddBlocks: UnknownRegionsWithOddBlockSize,
      stateCounts: StateCounts
    ): Option[SolutionStep] =
      oddBlocks.values.collectFirst { case (lineId, unk) if unk.size == 1 =>
        val region = unk.headOption.get
        val nNeeded = stateCounts.values(lineId).blackNeeded
        val parity = if nNeeded % 2 == 1 then "odd" else "even"
        val state = if stateCounts.values(lineId).blackNeeded % 2 == 1 then RegionState.Black else RegionState.White
        val d = desc"In {l $lineId | Line with One Odd Unknown Group}, the only unknown region with an odd number of "
          ++ desc"spaces is {c $region | Region with Odd # of Spaces}. As this line needs an $parity number ($nNeeded) of "
          ++ desc"Black spaces, this region must be $state."
        SolutionStep(Explanation("Parity", d), List(Move(region, state)))
      }
