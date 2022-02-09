package dev.nrp.puzzles.crossapix.app

import cats.{Functor, Monad}
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import dev.nrp.puzzles.crossapix.attributes.Attributes
import dev.nrp.puzzles.crossapix.game.*
import dev.nrp.puzzles.crossapix.heuristics.Heuristic

/**
 * Trait demonstrating that for any choice of `Attrs`, `F[Attrs, _]` is a monadic CrossAPix application.
 *
 * This is designed to make implementing state, side effects, IO, etc as absolutely low on boilerplate as possible.
 * To wit, this enables the [[CAPApp.runApp]] method to handle absolutely everything, given the basic logic encoded
 * in the [[CAPApp]] instance and a pluggable [[dev.nrp.puzzles.crossapix.heuristics.Heuristic]].
 *
 * Note that this is extremely flexible: a simple command-line readout of moves and a full-fledged interactive GUI
 * (which can even include the user providing moves in between hints!) can both be naturally represented by these same
 * few methods and the right choice of monadic effects. The game loop handles everything else.
 *
 * @tparam Attrs The type of the [[dev.nrp.puzzles.crossapix.attributes.Attributes]] tracked through the application.
 *               Will generally be determined by the chosen [[dev.nrp.puzzles.crossapix.heuristics.Heuristic]] (see
 *               the [[CAPApp.runApp]] method).
 * @tparam F Bivariate parameterized type such that `F[Attrs, _]` is a monad representing the desired mode of
 *           computation
 */
trait CAPApp[Attrs, F[_, _]] extends Monad[[T] =>> F[Attrs, T]]:
  /**
   * Program that performs any needed initialization.
   *
   * Note that this can include waiting for user input, including allowing the user to make some moves -- in this case,
   * they should be yielded as the result.  (Yielding an empty list of moves means the heuristic will start making
   * move suggestions directly from the initial board.)
   */
  def initialize: F[Attrs, List[Move]]

  /** Retrieve the game attributes tracked by this application (generally as state for the heuristics). */
  def getAttributes: F[Attrs, Attrs]

  /** Update the game attributes (generally following a move changing the board state) */
  def modifyAttributes(f: Attrs => Attrs): F[Attrs, Unit]

  /**
   * Program that performs any actions needed based on a [[dev.nrp.puzzles.crossapix.game.SolutionStep]] produced by
   * the heuristic, for instance storing statistics, highlighting relevant regions/lines, or printing the heuristic's
   * explanation.
   *
   * Note that this SHOULD NOT handle the udpate of the `Attrs` -- that heuristic logic is encoded in the game loop
   * already.
   */
  def handleStepAndGetInput(step: SolutionStep): F[Attrs, List[Move]]

  /** Retrieve the [[dev.nrp.puzzles.crossapix.game.Board]] as a monadic action. */
  def getBoard: F[Attrs, Board]

  /** Monadic program for reacting to the end of the game.  Could display statistics etc. */
  def close: F[Attrs, Unit]

  /**
   *  Execute a monadic program against a given board, yielding a basic [[cats.effect.IO]] representing the final
   *  program.
   */
  def run[T](app: F[Attrs, T], board: Board, attrs: Attrs): IO[T]

object CAPApp:
  /**
   * Build and execute the game loop against a given board, with a specified heuristic solver and monadic [[CAPApp]].
   *
   * Combines the solver logic of the heuristic, the game logic encoded here, and the application logic encoded into the
   * [[CAPApp]] into an actual, running application.
   *
   * @param board Board on which the game is played
   * @tparam H Type that is a member of the [[dev.nrp.puzzles.crossapix.heuristics.Heuristic]] typeclass, representing
   *           the game solver logic.
   * @tparam F Type that is a member of the [[CAPApp]] typeclass (for the attributes for the heuristic), encoding the
   *           application logic.
   * @return A [[cats.effect.IO]] program representing the entire application.
   */
  def runApp[H, F[_, _]](using
    h: Heuristic[H],
    c: CAPApp[h.Attrs, F],
    a: Attributes[h.Attrs]
  )(board: Board): IO[Unit] =
    def applyMoves(board: Board)(moves: List[Move])(curAttrs: h.Attrs): h.Attrs = 
      moves.foldLeft(curAttrs) { (cur, mv) => a.update(board, mv, cur) }

    val stepper: OptionT[[A] =>> F[h.Attrs, A], Unit] =
      for {
        board <- OptionT.liftF(c.getBoard)
        attrs <- OptionT.liftF(c.getAttributes)
        step <- OptionT.fromOption(h.nextStep(attrs))
        _ <- OptionT.liftF(c.modifyAttributes(applyMoves(board)(step.moves)))
        manualMoves <- OptionT.liftF(c.handleStepAndGetInput(step))
        _ <- OptionT.liftF(c.modifyAttributes(applyMoves(board)(manualMoves)))
      } yield ()

    val runner: F[h.Attrs, Unit] =
      for {
        initialUserMoves <- c.initialize
        _ <- c.modifyAttributes(applyMoves(board)(initialUserMoves))
        _ <- LazyList.continually(stepper).sequence_.value.map(_ => None)
        _ <- c.close
      } yield ()
    
    c.run(runner, board, a.initialize(board))

