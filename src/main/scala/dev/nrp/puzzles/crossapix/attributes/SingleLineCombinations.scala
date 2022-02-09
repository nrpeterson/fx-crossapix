package dev.nrp.puzzles.crossapix.attributes

import cats.Eval
import dev.nrp.puzzles.crossapix.game.*
import monocle.{Focus, Lens}
import monocle.syntax.all.*

import scala.annotation.tailrec

/**
 * A collection of metrics that enable computation of all combos of regions in a given line that yield the right total.
 *
 * @param totalUnknown Number of spaces (individual grid boxes) in this line not marked Black or White
 * @param totalUnknownBlocks Number of blocks (spaces in which a region intersects this line) not marked Black or White
 * @param blackNeeded Number of Black spaces needed to finish this line (total black needed less number marked Black)
 * @param sizeToRegions Map from sizes of unknown blocks to the collection of regions that intersect this line in that
 *                    block size. Note that when updated, block sizes that no longer have any regions should be removed.
 * @param combs       [[cats.Eval]] instance that returns all possible combinations of regions that could be marked with
 *                    [[LineCombos.combsColor]] to finalize this region, as `size->#` pairs.
 * @param combsColor [[dev.nrp.puzzles.crossapix.game.RegionState]] representing the color for which [[LineCombos.combs]]
 *                   will solve this line.  Note that we choose color based on which color needs fewer to finish.
 */
case class LineCombos(
  totalUnknown: Int,
  totalUnknownBlocks: Int,
  blackNeeded: Int,
  sizeToRegions: Map[Int, List[RegionId]],
  combs: Eval[List[List[(Int, Int)]]],
  combsColor: RegionState
)

/**
 * Line-by-line metrics relating to possible combinations of regions for completing a single line.
 *
 * This is just a wrapper around a `Map[LineId, LineCombos`, which admits a [[LineAttribute]] instance.
 *
 * @param values The wrapped mapping from [[dev.nrp.puzzles.crossapix.game.LineId]]s to [[LineCombos]] objects.
 */
case class SingleLineCombinations(values: Map[LineId, LineCombos])

object SingleLineCombinations:

  /**
   * Find all combinations of region sizes that sum to the given total, given available sizes and multiplicities.
   *
   * Basically the 'change problem': given a collection of coins, what combinations of coins make the given value?
   *
   * @param total Desired total
   * @param sizeToCount List of `size-># available` pairs. Note that the sizes should be distinct.
   * @return List of combinations, where each combination takes the form of a list of `(size, #)` pairs.
   */
  def combinations(total: Int, sizeToCount: List[(Int, Int)]): List[List[(Int, Int)]] =
    assert(sizeToCount.map(_._1).distinct.length == sizeToCount.length)

    @tailrec
    def combsR(
      sizeToCount: List[(Int, Int)],
      combs: List[(Int, List[(Int, Int)])]
    ): List[List[(Int, Int)]] = sizeToCount match
      case Nil => combs.collect { case (0, cs) => cs }
      case List(i->count) =>
        combs.collect { case (r, cs) if r % i == 0 && r <= i * count => (i, r / i) :: cs }
      case (i->count) :: rest =>
        val newResults = combs.flatMap { case (r, cs) =>
          (0 to math.min(count, r/i)).map { n => (r - (n * i), (i, n) :: cs) }
        }
        combsR(rest, newResults)

    if sizeToCount.isEmpty then List.empty
    else
      val sorted = sizeToCount.sortBy( _._1 * -1)
      combsR(sorted, List((total, List.empty)))

  /**
   * Build all possible combinations for solving a single line from total unknown, black needed, and group sizes.
   *
   * This problem can be viewed equally validly as being to choose which groups will be black or which regions will be
   * white; so, this method solves the problem from whichever angle produces a smaller total for optimization reasons.
   *
   * @param totalUnknown Total number of unknown spaces in this line
   * @param blackNeeded Of the unknown spaces, how many more need to be marked black?
   * @param sizeToRegions Map from group size to a list of regions intersecting this line in that group size.
   * @return A pair `(combinations, desired color)`, where:
   *         - `desired color` is a [[dev.nrp.puzzles.crossapix.game.RegionState]] representing whether we are finding
   *         which regions to mark white or which regions to mark black
   *         - `combinations` is a [[cats.Eval]] representing lazily computing all possible combinations for the
   *         desired color.
   */
  def buildLineCombinations(
    totalUnknown: Int,
    blackNeeded: Int,
    sizeToRegions: Map[Int, List[RegionId]]
  ): (Eval[List[List[(Int, Int)]]], RegionState) =
    if totalUnknown == 0 then (Eval.now(List.empty[List[(Int, Int)]]), RegionState.Black)
    else
      val (combsColor, totalNeeded) =
        if 2 * blackNeeded <= totalUnknown then (RegionState.Black, blackNeeded)
        else (RegionState.White, totalUnknown - blackNeeded)

      val combs = Eval.later {
        val sizeToCount = sizeToRegions.map { case (i, ls) => (i, ls.size) }
        combinations(totalNeeded, sizeToCount.toList)
      }
      (combs, combsColor)


  /**
   * [[LineAttribute]] instance for leveraging single line combination info as an attribute.
   */
  given lineCombosAttribute: LineAttribute[SingleLineCombinations, LineCombos] with
    // TODO This redoes calculations done in other attributes.  Time to build a compute graph...

    /** [[monocle.Lens]] for zooming in from a [[SingleLineCombinations]] to its inner map. */
    val map: Lens[SingleLineCombinations, Map[LineId, LineCombos]] = Focus[SingleLineCombinations](_.values)

    /** Construct a [[SingleLineCombinations]] from its inner map. */
    def construct(values: Map[LineId, LineCombos]): SingleLineCombinations = SingleLineCombinations(values)

    /** Initialize the given line to represent line combos for the given board. */
    def initializeLine(board: Board, lineId: LineId): LineCombos =
      val sizeToRegions: Map[Int, List[RegionId]] = board.lineView(lineId)
        .groupMap(_._2)(_._1)
        .map { case (size, regions) => (size, regions.toList) }

      val blackNeeded = board.lineBlackTotal(lineId)
      val totalUnknown = board.lineLength(lineId)
      val totalUnknownBlocks = board.lineView(lineId).size
      val (combs, combsColor) = buildLineCombinations(totalUnknown, blackNeeded, sizeToRegions)

      LineCombos(totalUnknown, totalUnknownBlocks, blackNeeded, sizeToRegions, combs, combsColor)

    /** Update line combinations for a line impacted by a new move. */
    def updateImpactedLine(board: Board, move: Move, lineId: LineId, curValue: LineCombos): LineCombos =
      // TODO We can do this more efficiently in some cases by updating the Eval instead of replacing it
      val regionSize = board.lineView.apply(lineId)(move.regionId)
      val newTotalUnknown = curValue.totalUnknown - regionSize
      val newBlackNeeded = move.state match
        case RegionState.Black => curValue.blackNeeded - regionSize
        case RegionState.White => curValue.blackNeeded
      val newTotalUnknownBlocks = curValue.totalUnknownBlocks - 1

      val newSToC = curValue.sizeToRegions
        .focus(_.index(regionSize))
        .modify(_.filterNot(_ == move.regionId))
        .filterNot(_._2.isEmpty)

      val (newCombs, newCombsColor) = buildLineCombinations(newTotalUnknown, newBlackNeeded, newSToC)

      LineCombos(newTotalUnknown, newTotalUnknownBlocks, newBlackNeeded, newSToC, newCombs, newCombsColor)
