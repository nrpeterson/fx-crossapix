package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.game.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class UnknownRegionsByBlockSizeSpec extends AnyFlatSpec with Matchers {
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

  val instance = summon[LineAttribute[UnknownRegionsByBlockSize, LineUnknownRegionsByBlockSize]]

  "LineAttribute[UnknownRegionsByBlockSize]" should "initialize correctly" in {
    val result1 = instance.initializeLine(board, LineId.Col(0)).sizeToRegions
    result1.keySet should contain theSameElementsAs List(1, 2, 3)
    result1(1) should contain theSameElementsAs List(13, 20)
    result1(2) should contain theSameElementsAs List(0)
    result1(3) should contain theSameElementsAs List(24)
  }

  it should "only update impacted lines" in {
    val move = Move(24, RegionState.White)
    val impactedLines = Set(LineId.Row(4), LineId.Row(5), LineId.Row(6), LineId.Col(0))

    val init = instance.initialize(board)
    val updated = instance.update(board, move, init)

    board.lines.foreach { line =>
      if impactedLines.contains (line) then updated.values (line) shouldNot equal (init.values (line))
      else updated.values (line) shouldEqual init.values (line)
    }
  }

  it should "remove a region after its state is set" in {
    val move = Move(24, RegionState.White)
    val init = instance.initialize(board)
    val updated = instance.update(board, move, init)
    updated.values.foreach { case (lineId, u) =>
      u.sizeToRegions.flatMap(_._2) shouldNot contain (24)
    }
  }

  it should "remove a size from a line after all regions of that size in that line are removed" in {
    val move = Move(24, RegionState.White)
    val init = instance.initialize(board)
    val updated = instance.update(board, move, init)

    val line = LineId.Col(0)

    init.values(line).sizeToRegions should contain key 3
    init.values(line).sizeToRegions(3) shouldEqual List(24)
    updated.values(line).sizeToRegions shouldNot contain key 3
  }
}
