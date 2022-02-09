package dev.nrp.puzzles.crossapix.heuristics

import cats.Eval
import dev.nrp.puzzles.crossapix.attributes.{LineCombos, SingleLineCombinations}
import dev.nrp.puzzles.crossapix.game.*
import dev.nrp.puzzles.crossapix.game.RegionState.opposite

trait PossibleLineCombos

object PossibleLineCombos:
  def findDecisions(lineId: LineId, lineCombs: LineCombos): Eval[Option[SolutionStep]] = lineCombs.combs.map { combs =>
    val allOrNothing = combs.flatten
      .groupMapReduce(_._1) { case (_, count) => Set(count) } { (s1, s2) => s1.union(s2) }
      .collect { case (i, counts) if counts.size == 1 => (i, counts.head) }
      .filter { case (i, count) => count == 0 || count == lineCombs.sizeToRegions(i).length }

    if allOrNothing.isEmpty then None
    else
      val mvs = allOrNothing.flatMap { case (i, c) =>
        val st = if c == 0 then opposite(lineCombs.combsColor) else lineCombs.combsColor
        lineCombs.sizeToRegions(i).map { regionId => Move(regionId, st) }
      }

      val st = lineCombs.combsColor
      val opSt = opposite(st)
      val n = if st == RegionState.Black then lineCombs.blackNeeded else lineCombs.totalUnknown - lineCombs.blackNeeded

      val dChoices = allOrNothing
        .map { case (i, c) =>
          if c == 0 then desc"- All groups of size $i are marked $opSt"
          else desc"- All groups of size $i are marked $st"
        }
        .reduce( (l, r) => l ++ (Text("\n") :: r) )

      val d = desc"{l $lineId | Line in Question} needs $n more $st spaces. "
        ++ desc"Given the sizes of space groups in this line, this is only possible if:\n"
        ++ dChoices

      Some(SolutionStep(Explanation("Possible Combos", d), mvs.toList))
  }

  given Heuristic1[PossibleLineCombos] with
    type Attr = SingleLineCombinations

    def getStep(combos: SingleLineCombinations): Option[SolutionStep] =
      combos.values.toList
        .sortBy { case (lineId, stats) => stats.totalUnknownBlocks }
        .map(findDecisions.tupled)
        .collectFirst { case eval if eval.value.isDefined => eval.value.get }