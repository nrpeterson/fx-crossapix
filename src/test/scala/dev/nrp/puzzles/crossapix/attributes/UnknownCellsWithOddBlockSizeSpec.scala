/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.*
import dev.nrp.puzzles.crossapix.game.*
import org.scalacheck.Gen

class UnknownRegionsWithOddBlockSizeSpec extends Spec {
  val (board, _) = Board.fromGrid(
    List(
      List( 0, 1, 2, 3, 4, 5, 6),
      List( 0, 7, 8, 9,10,11,12),
      List(13,14,15,16,17,18,19),
      List(20,21,15,22,17,23,19),
      List(24,25,26,27,28,29,30),
      List(24,31,32,33,34,35,36),
      List(24,37,38,39,40,40,41)
    ),
    Vector(2, 3, 3, 5, 4, 5, 5),
    Vector(2, 6, 6, 6, 4, 1, 2)
  )

  val instance = LineAttribute[UnknownRegionsWithOddBlockSize, Set[RegionId]]

  val moveGen: Gen[Move] = for {
    regionId <- Gen.oneOf(board.regions.keys)
    st <- Gen.oneOf(RegionState.Black, RegionState.White)
  } yield Move(regionId, st)

  val lineGen: Gen[LineId] =
    val rowGen = Gen.oneOf(0 until board.numRows).map(LineId.Row.apply)
    val colGen = Gen.oneOf(0 until board.numCols).map(LineId.Col.apply)
    Gen.oneOf(rowGen, colGen)

  def impactedLineGen(move: Move): Gen[LineId] =
    val choices =
      board.regions(move.regionId)
        .flatMap { case Space(row, col) => List(LineId.Row(row), LineId.Col(col)) }
        .distinct

    Gen.oneOf(choices)

  val curGen: Gen[UnknownRegionsWithOddBlockSize] =
    Gen.listOf(moveGen)
      .map(_.distinctBy(_.regionId))
      .map { (moves: List[Move]) =>
        val init = Attribute[UnknownRegionsWithOddBlockSize].initialize(board)
        moves.foldLeft(init) { (cur, mv) => instance.update(board, mv, cur) }
      }

  "LineAttribute[UnknownRegionsWithOddBlockSize].initializeLine" should "work" in {
    val fullResult = instance.initialize(board)

    forAll(lineGen) { lineId =>
      val result = instance.initializeLine(board, lineId)
      val expected =
        board.lineView(lineId)
          .collect { case (regionId, size) if size % 2 == 1 => regionId }
          .toSet

      result shouldEqual expected
      result shouldEqual fullResult.values(lineId)
    }
  }

  "LineAttribute[UnknownRegionsWithOddBlockSize].updateImpactedLine" should "work!" in {
    val init = instance.initialize(board)
    val gen = for {
      prevMoves <- Gen.listOf(moveGen).map(_.distinctBy(_.regionId))
      seenRegions = prevMoves.map(_.regionId).toSet
      cur = prevMoves.foldLeft(init) { (cur, mv) => instance.update(board, mv, cur) }
      move <- moveGen.suchThat( mv => !seenRegions.contains(mv.regionId) )
      lineId <- Gen.oneOf(board.linesForRegion(move.regionId))
    } yield (cur.values(lineId), move, lineId)

    forAll(gen) { case (cur: Set[RegionId], move: Move, line: LineId) =>
      val result = instance.updateImpactedLine(board, move, line, cur)

      if board.lineView(line)(move.regionId) % 2 == 0 then result shouldEqual cur
      else
        result.size shouldEqual (cur.size - 1)
        cur should contain (move.regionId)
        result shouldNot contain (move.regionId)
    }
  }
}
