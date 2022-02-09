package dev.nrp.puzzles.crossapix.heuristics

import dev.nrp.puzzles.crossapix.attributes.{Attribute, Attributes}
import dev.nrp.puzzles.crossapix.game.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class HeuristicSpec extends AnyFlatSpec with Matchers {
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
    def initialize(board: Board): String = "|"
    def update(board: Board, move: Move, curValue: String): String = f"$curValue${move.regionId}|"

  trait H1
  trait H2
  trait H3

  given Heuristic.Aux[H1, EmptyTuple] = new Heuristic[H1]:
    type Attrs = EmptyTuple
    def nextStep(attrs: EmptyTuple): Option[SolutionStep] = None

  given Heuristic.Aux[H2, (Int, String)] = new Heuristic[H2]:
    type Attrs = (Int, String)
    def nextStep(attrs: (Int, String)): Option[SolutionStep] = Some(
      SolutionStep(
        Explanation("H2", List.empty),
        List(Move(24, RegionState.White), Move(13, RegionState.White))
      )
    )

  given Heuristic.Aux[H3, (Board, Int)] = new Heuristic[H3]:
    type Attrs = (Board, Int)
    def nextStep(attrs: (Board, Int)): Option[SolutionStep] = Some(
      SolutionStep(
        Explanation("H3", List.empty),
        List(Move(24, RegionState.White))
      )
    )

  def decisionAfter[H](using h: Heuristic[H], a: Attributes[h.Attrs])(moves: List[Move]): (h.Attrs, Option[SolutionStep]) =
    val init = a.initialize(board)
    val cur = moves.foldLeft(init) { (last, move) => a.update(board, move, last) }
    (cur, h.nextStep(cur))

  "Heuristic for tuples" should "correctly initialize and update distinct attribute list" in {
    val (initAttrs, _) = decisionAfter[(H1, H2, H3)](List.empty)

    val initInt = Attribute[Int].initialize(board)
    val initString = Attribute[String].initialize(board)
    val initBoard = Attribute[Board].initialize(board)

    initAttrs shouldEqual (initInt, initString, initBoard)

    val move = Move(24, RegionState.White)
    val nextInt = Attribute[Int].update(board, move, initInt)
    val nextString = Attribute[String].update(board, move, initString)
    val nextBoard = Attribute[Board].update(board, move, initBoard)

    val (nextAttrs, _) = decisionAfter[(H1, H2, H3)](List(move))
    nextAttrs shouldEqual (nextInt, nextString, nextBoard)
  }

  it should "draw from the heuristics in order until it finds a move" in {
    val (_, result) = decisionAfter[(H1, H2, H3)](List.empty)

    result shouldBe defined
    val moves = result.get.moves

    moves shouldEqual List(Move(24, RegionState.White), Move(13, RegionState.White))
  }
}
