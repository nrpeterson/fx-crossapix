package dev.nrp.puzzles.crossapix.game

import dev.nrp.puzzles.crossapix.utils.ListUtils
import monocle.syntax.all.*

import scala.collection.immutable.SortedMap

enum RegionState:
  case Black
  case White

object RegionState:
  def opposite(st: RegionState): RegionState = st match
    case Black => White
    case White => Black

enum LineId:
  case Row(i: Int)
  case Col(j: Int)

object LineId:
  def toTuple(lineId: LineId): (Int, Int) = lineId match
    case Row(i) => (0, i)
    case Col(j) => (1, j)

  given (using ev: Ordering[(Int, Int)]): Ordering[LineId] with
    override def compare(x: LineId, y: LineId): Int = ev.compare(toTuple(x), toTuple(y))

type RegionId = Int

case class Space(row: Int, col: Int):
  def toLines: List[LineId] = List(LineId.Row(row), LineId.Col(col))

case class Board(
  numRows: Int,
  numCols: Int,
  rowTotals: Vector[Int],
  colTotals: Vector[Int],
  regions: Map[RegionId, List[Space]]
):
  val lineView: Map[LineId, Map[RegionId, Int]] =
    val pairs = regions.toList.flatMap { case (c, spaces) => spaces.flatMap(_.toLines).map( (_, c)) }
    SortedMap.from(ListUtils.hierarchyCounts(pairs))

  val lines: List[LineId] = 
    (0 until numRows).toList.map(LineId.Row.apply) ++ (0 until numCols).toList.map(LineId.Col.apply)
  
  val linesForRegion: Map[RegionId, List[LineId]] =
    def blowUp(spaces: List[Space]): List[LineId] =
      spaces.flatMap { case Space(r, c) => List(LineId.Row(r), LineId.Col(c)) }
      
    regions.map { case (c, spaces) => (c, blowUp(spaces).distinct) }

  def lineBlackTotal(lineId: LineId): Int = lineId match
    case LineId.Row(i) => rowTotals(i)
    case LineId.Col(j) => colTotals(j)

  def lineLength(lineId: LineId): Int = lineId match
    case LineId.Row(_) => numCols
    case LineId.Col(_) => numRows

case class BoardState(states: Map[Int, Option[RegionState]])

object Board:
  def fromGrid(grid: List[List[Int]], rowTotals: Vector[Int], colTotals: Vector[Int]): (Board, BoardState) =
    val numRows = grid.length
    val numCols = grid.head.length
    val regions: Map[Int, List[Space]] = SortedMap.from(
      ListUtils.zipWithIndex2(grid)
        .groupMap { case (_, c) => c } { case ((i, j), _) => Space(i, j) }
        .map { case (c, spaces) => (c, spaces.toList) }
    )
  
    val board = Board(numRows, numCols, rowTotals, colTotals, regions)
    val boardState = BoardState(regions.map { case (c, _) => (c, None)})
    (board, boardState)

  def toPicture(board: Board, boardState: BoardState): String =
    val original: List[List[Char]] = List.fill(board.numRows, board.numCols)('*')
    val changes = board.regions
      .flatMap { case (c, spaces) =>
        val st = boardState.states(c)
        spaces.map { (_, st) }
      }
  
    val result = changes.foldLeft(original) {
      case (prev, (Space(r, c), st)) =>
        val char = st match {
          case None => '.'
          case Some(RegionState.Black) => 'X'
          case Some(RegionState.White) => ' '
        }
        prev.focus(_.index(r).index(c)).replace(char)
    }
  
    result.map(_.mkString("..")).mkString("\n")


def applyMove(move: Move, boardState: BoardState): BoardState = boardState.focus(_.states.index(move.regionId)).replace(Some(move.state))