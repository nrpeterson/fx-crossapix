package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.*
import dev.nrp.puzzles.crossapix.game.*
import org.scalacheck.Gen

class AttributesSpec extends Spec {
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

  given Attribute[Int] with
    def initialize(board: Board): Int = 0
    def update(board: Board, move: Move, curValue: Int): Int = curValue + 1

  given Attribute[String] with
    def initialize(board: Board): String = ""
    def update(board: Board, move: Move, curValue: String): String = f"$curValue|${move.regionId}"

  val instance: Attributes[(Int, String)] = summon

  "Attributes.initialize" should "delegate to initializers for each attribute" in {
    val result = instance.initialize(board)
    result shouldEqual (0, "")
  }

  "Attributes.update" should "update both attributes" in {
    val init = instance.initialize(board)
    val move1 = Move(24, RegionState.White)
    val result1 = instance.update(board, move1, init)

    result1 shouldEqual (1, "|24")

    val move2 = Move(13, RegionState.Black)
    val result2 = instance.update(board, move2, result1)

    result2 shouldEqual (2, "|24|13")
  }

  "Attribute[Board]" should "be a no-op" in {
    Attribute[Board].initialize(board) shouldEqual board

    val gen = for {
      regionId <- Gen.oneOf(board.regions.keys)
      st <- Gen.oneOf(RegionState.Black, RegionState.White)
    } yield Move(regionId, st)

    forAll(gen) { mv =>
      Attribute[Board].update(board, mv, board) shouldEqual board
    }
  }

  "Attribute[BoardState]" should "initialize BoardState with all regions and no states" in {
    val result = Attribute[BoardState].initialize(board)

    result.states.keys should contain theSameElementsAs board.regions.keys
  }

  it should "update by applying moves" in {
    val moveGen = for {
      region <- Gen.oneOf(board.regions.keySet)
      st <- Gen.oneOf(RegionState.Black, RegionState.White)
    } yield Move(region, st)

    val gen = Gen.listOf(moveGen).map(_.distinctBy(_.regionId))

    forAll(gen) { (moves: List[Move]) =>
      val initialState = Attribute[BoardState].initialize(board)

      val expected = moves.foldLeft(initialState) { (st, mv) => applyMove(mv, st) }
      val actual = moves.foldLeft(initialState) { (st, mv) => Attribute[BoardState].update(board, mv, st) }

      expected shouldEqual actual
    }
  }
}
