package dev.nrp.puzzles.crossapix.attributes

import dev.nrp.puzzles.crossapix.game.{Board, Move}

/**
 * Typeclass witnessing that every element of the tuple [[T]] is an [[Attribute]].
 *
 * @tparam T Tuple whose coordinates are all [[Attribute]]s.
 */
trait Attributes[T <: Tuple]:
  /**
   * Initialize the tuple [[T]] by initializing each coordinate using its [[Attribute]] instance.
   *
   * @param board [[dev.nrp.puzzles.crossapix.game.Board]] to use for initialization
   * @return Instance of [[T]] containing initial values for all attributes.
   */
  def initialize(board: Board): T

  /**
   * Update every coordinate of the tuple [[T]] using the [[Attribute]] instance for its type.
   *
   * @param board [[dev.nrp.puzzles.crossapix.game.Board]] representing the puzzle structure
   * @param move A new [[dev.nrp.puzzles.crossapix.game.Move]] to use for updating
   * @param curValue Current [[T]] attributes before this new [[dev.nrp.puzzles.crossapix.game.Move]]
   * @return Updated values for all coordinates of the `curValue` given the [[dev.nrp.puzzles.crossapix.game.Move]]
   */
  def update(board: Board, move: Move, curValue: T): T

object Attributes:
  /** Summon the [[Attributes]] instance for the given tuple type `T`. */
  def apply[T <: Tuple](using ev: Attributes[T]): Attributes[T] = ev

  /** [[EmptyTuple]] is the base case for recursively generating [[Attributes]]: the value is always [[EmptyTuple]] */
  given emptyTupleAttributes: Attributes[EmptyTuple] with
    def initialize(board: Board): EmptyTuple = EmptyTuple
    def update(board: Board, move: Move, curValue: EmptyTuple): EmptyTuple = EmptyTuple

  /** Recursively generate an [[Attributes]] given a head [[Attribute]] and a tail [[Attributes]]. */
  given recursiveTupleAttributes[H : Attribute, T <: Tuple : Attributes]: Attributes[H *: T] with
    def initialize(board: Board): H *: T = Attribute[H].initialize(board) *: Attributes[T].initialize(board)
    def update(board: Board, move: Move, curValue: H *: T): H *: T = curValue match
      case h *: t =>
        val newH = Attribute[H].update(board, move, h)
        val newT = Attributes[T].update(board, move, t)
        newH *: newT
