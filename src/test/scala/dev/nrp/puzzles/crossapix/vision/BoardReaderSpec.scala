/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.vision

import java.io.File

import cats.effect.testing.scalatest.*
import dev.nrp.puzzles.crossapix.*
import dev.nrp.puzzles.crossapix.game.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.*

import scala.math.Ordering

class BoardReaderSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "BoardReader.parseBoardFromFile" should "... work!" in {
    val file = File(getClass.getResource("/32090100378.png").getPath)

    BoardReader.parseBoardFromFile(file).asserting { board =>
      board.numRows shouldEqual 10
      board.numCols shouldEqual 10

      board.rowTotals shouldEqual Vector(3, 3, 3, 2, 9, 8, 8, 8, 6, 5)
      board.colTotals shouldEqual Vector(3, 5, 5, 7, 8, 7, 8, 6, 2, 4)
      
      val expectedRegions: Map[RegionId, List[Space]] = Map(
        0 -> List((0, 0), (0, 1), (0, 2)),
        1 -> List((0, 3)),
        2 -> List((0, 4)),
        3 -> List((0, 5)),
        4 -> List((0, 6)),
        5 -> List((0, 7)),
        6 -> List((0, 8), (0, 9)),
        7 -> List((1, 0)),
        8 -> List((1, 1)),
        9 -> List((1, 2), (2, 2)),
        10 -> List((1, 3), (2, 3)),
        11 -> List((1, 4), (2, 4)),
        12 -> List((1, 5)),
        13 -> List((1, 6), (2, 6)),
        14 -> List((1, 7), (2, 7), (3, 7)),
        15 -> List((1, 8), (1, 9)),
        16 -> List((2, 0), (2, 1)),
        17 -> List((2, 5)),
        18 -> List((2, 8), (2, 9)),
        19 -> List((3, 0), (3, 1)),
        20 -> List((3, 2), (3, 3)),
        21 -> List((3, 4), (3, 5), (3, 6)),
        22 -> List((3, 8), (3, 9), (4, 9), (5, 9)),
        23 -> List((4, 0), (5, 0), (6, 0)),
        24 -> List((4, 1)),
        25 -> List((4, 2)),
        26 -> List((4, 3)),
        27 -> List((4, 4)),
        28 -> List((4, 5)),
        29 -> List((4, 6)),
        30 -> List((4, 7)),
        31 -> List((4, 8), (5, 8)),
        32 -> List((5, 1)),
        33 -> List((5, 2), (6, 2)),
        34 -> List((5, 3), (5, 4)),
        35 -> List((5, 5)),
        36 -> List((5, 6), (5, 7)),
        37 -> List((6, 1), (7, 1)),
        38 -> List((6, 3)),
        39 -> List((6, 4), (6, 5)),
        40 -> List((6, 6), (7, 6)),
        41 -> List((6, 7)),
        42 -> List((6, 8)),
        43 -> List((6, 9)),
        44 -> List((7, 0)),
        45 -> List((7, 2)),
        46 -> List((7, 3)),
        47 -> List((7, 4)),
        48 -> List((7, 5)),
        49 -> List((7, 7), (7, 8)),
        50 -> List((7, 9), (8, 9)),
        51 -> List((8, 0)),
        52 -> List((8, 1)),
        53 -> List((8, 2)),
        54 -> List((8, 3)),
        55 -> List((8, 4), (8, 5)),
        56 -> List((8, 6), (9, 6)),
        57 -> List((8, 7)),
        58 -> List((8, 8)),
        59 -> List((9, 0), (9, 1)),
        60 -> List((9, 2), (9, 3)),
        61 -> List((9, 4), (9, 5)),
        62 -> List((9, 7)),
        63 -> List((9, 8), (9, 9))
      ).map { case (k, v) => (k, v.map(Space.apply.tupled)) }

      assert(expectedRegions.flatMap(_._2).size == 100)

      val actualRegions = board.regions

      actualRegions.size shouldEqual expectedRegions.size

      def groups(regionMap: Map[RegionId, List[Space]]): List[List[Space]] =
        regionMap.values.toList
          .sortBy { list => list.map(space => (space. row, space.col)).min }
          .map { list => list.sortBy( space => (space.row, space.col)) }

      groups(actualRegions) shouldEqual groups(expectedRegions)
    }
  }
}
