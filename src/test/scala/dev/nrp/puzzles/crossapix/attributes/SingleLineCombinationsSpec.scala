/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.*
import dev.nrp.puzzles.crossapix.game.*
import org.scalacheck.Gen

class SingleLineCombinationsSpec extends Spec {
  val instance = LineAttribute[SingleLineCombinations, LineCombos]

  "SingleLineCombinations.combinations" should "work!" in {
    val sizeToCount = List(1->2, 2->1, 3->3, 6->1)
    val result = SingleLineCombinations.combinations(7, sizeToCount)

    val expected = List(
      List(1->1, 2->0, 3->0, 6->1),
      List(1->1, 2->0, 3->2, 6->0),
      List(1->2, 2->1, 3->1, 6->0)
    )

    result should contain theSameElementsAs expected
  }

  "LineAttribute[SingleLineCombinations, LineCombos].initializeLine" should "work!" in {
    val (board, _) = Board.fromGrid(
      List(
        List(1, 2, 2),
        List(3, 4, 5),
        List(3, 6, 7)
      ),
      Vector(2, 1, 2),
      Vector(1, 1, 1)
    )

    val result1 = instance.initializeLine(board, LineId.Row(0))
    result1.totalUnknown shouldEqual 3
    result1.totalUnknownBlocks shouldEqual 2
    result1.blackNeeded shouldEqual 2
    result1.sizeToRegions shouldEqual Map(1->List(1), 2->List(2))
    result1.combsColor shouldEqual RegionState.White

    val combs1 = result1.combs.value
    combs1.length shouldEqual 1
    combs1.head shouldEqual List(1->1, 2->0)

    val result2 = instance.initializeLine(board, LineId.Col(0))
    result2.totalUnknown shouldEqual 3
    result2.totalUnknownBlocks shouldEqual 2
    result2.blackNeeded shouldEqual 1
    result2.sizeToRegions shouldEqual Map(1->List(1), 2->List(3))
    result2.combsColor shouldEqual RegionState.Black

    val combs2 = result2.combs.value
    combs2.length shouldEqual 1
    combs2.head shouldEqual List(1->1, 2->0)
  }
}
