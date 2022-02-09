package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.game.*

/**
 * Typeclass witnessing that a type can be initialized from a [[dev.nrp.puzzles.crossapix.game.Board]], and updated
 * from a [[dev.nrp.puzzles.crossapix.game.Move]].
 *
 * [[Attribute]]s represent a computable characteristic of a CrossAPix game, with initial state represented by
 * [[Attribute.initialize]] and stated updated after each move by [[Attribute.update]].
 *
 * For instance, one [[Attribute]] could be a map from [[dev.nrp.puzzles.crossapix.game.LineId]]s to the set of unknown
 * regions that intersect that line in an odd number of spaces, to power a
 * [[dev.nrp.puzzles.crossapix.heuristics.Heuristic]] related to parity of a line.
 *
 * @tparam A The type of the attribute in question. Note that any [[A]] must admit at most one [[Attribute[A]] instance.
 */
trait Attribute[A]:
  /**
   * Create an iniital attribute value from the given [[dev.nrp.puzzles.crossapix.game.Board]]
   *
   * @param board The [[dev.nrp.puzzles.crossapix.game.Board]] to use for initialization
   * @return Initial attribute state extracted from the given [[dev.nrp.puzzles.crossapix.game.Board]]
   */
  def initialize(board: Board): A

  /**
   * Update the [[Attribute]] from the current value, the [[dev.nrp.puzzles.crossapix.game.Board]], and a new
   * [[dev.nrp.puzzles.crossapix.game.Move]]
   *
   * @param board [[dev.nrp.puzzles.crossapix.game.Board]] (should match the [[dev.nrp.puzzles.crossapix.game.Board]] 
   *              used to initialize)
   * @param move A new [[dev.nrp.puzzles.crossapix.game.Move]] to react to
   * @param curValue The value of this attribute before the current [[dev.nrp.puzzles.crossapix.game.Move]]
   * @return Updated value of this [[Attribute]]
   */
  def update(board: Board, move: Move, curValue: A): A

object Attribute:
  /** Summon the given [[Attribute]] instance for the given type `A`. */
  def apply[A](using ev: Attribute[A]): Attribute[A] = ev

  /** The [[dev.nrp.puzzles.crossapix.game.Board]] is an [[Attribute]] which never changes. */
  given boardIsAttribute: Attribute[Board] with
    def initialize(board: Board): Board = board
    def update(board: Board, move: Move, curValue: Board): Board = board

  /**
   * The [[dev.nrp.puzzles.crossapix.game.BoardState]] is an [[Attribute]] which is updated by applying the new
   * [[dev.nrp.puzzles.crossapix.game.Move]]
   */
  given boardStateIsAttribute: Attribute[BoardState] with
    def initialize(board: Board): BoardState = BoardState(board.regions.map { case (c, _) => (c, None) })
    def update(board: Board, move: Move, curValue: BoardState): BoardState = applyMove(move, curValue)