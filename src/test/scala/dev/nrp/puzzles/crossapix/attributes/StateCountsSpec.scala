package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.game.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class StateCountsSpec extends AnyFlatSpec with Matchers:
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

  val instance = summon[LineAttribute[StateCounts, LineStateCounts]]

  "LineAttribute[StateCounts, LineStateCounts].initializeLine" should "work!" in {
    board.lines.foreach { line =>
      val result = instance.initializeLine(board, line)
      
      result.total shouldEqual board.lineLength(line)
      result.blackNeeded shouldEqual board.lineBlackTotal(line)
      result.whiteNeeded shouldEqual (board.lineLength(line) - board.lineBlackTotal(line))
      result.unknown shouldEqual result.total
      result.black shouldEqual 0
      result.white shouldEqual 0
    }
  }

  "LineAttribute[StateCounts, LineStateCounts].updateImpactedLine" should "work with Black moves" in {
    val move = Move(24, RegionState.Black)
    List(LineId.Row(4), LineId.Row(5), LineId.Row(6), LineId.Col(0)).foreach { line =>
      val init = instance.initializeLine(board, line)
      val updated = instance.updateImpactedLine(board, move, line, init)
      val nSpaces = board.lineView(line)(24)
      updated.total shouldEqual init.total
      updated.blackNeeded shouldEqual (init.blackNeeded - nSpaces)
      updated.whiteNeeded shouldEqual init.whiteNeeded
      updated.unknown shouldEqual (init.unknown - nSpaces)
      updated.black shouldEqual (init.black + nSpaces)
      updated.white shouldEqual init.white
    }
  }

  it should "work with White moves" in {
    val move = Move(24, RegionState.White)
    List(LineId.Row(4), LineId.Row(5), LineId.Row(6), LineId.Col(0)).foreach { line =>
      val init = instance.initializeLine(board, line)
      val updated = instance.updateImpactedLine(board, move, line, init)
      val nSpaces = board.lineView(line)(24)
      updated.total shouldEqual init.total
      updated.blackNeeded shouldEqual init.blackNeeded
      updated.whiteNeeded shouldEqual (init.whiteNeeded - nSpaces)
      updated.unknown shouldEqual (init.unknown - nSpaces)
      updated.black shouldEqual init.black
      updated.white shouldEqual (init.white + nSpaces)
    }
  }

  "LineAttribute[StateCounts, LineStateCounts].update" should "leave non-impacted lines alone" in {
    val move = Move(24, RegionState.White)
    val impactedLines = Set(LineId.Row(4), LineId.Row(5), LineId.Row(6), LineId.Col(0))

    val init = instance.initialize(board)
    val updated = instance.update(board, move, init)

    board.lines.foreach { line =>
      if impactedLines.contains(line) then updated.values(line) shouldNot equal (init.values(line))
      else updated.values(line) shouldEqual init.values(line)
    }
  }

  "LineStateCounts.needed" should "correctly fetch the number of needed regions per state" in {
    val lsc = LineStateCounts(10, 3, 4, 7, 2, 1)
    lsc.needed(RegionState.Black) shouldEqual lsc.blackNeeded
    lsc.needed(RegionState.White) shouldEqual lsc.whiteNeeded
  }
