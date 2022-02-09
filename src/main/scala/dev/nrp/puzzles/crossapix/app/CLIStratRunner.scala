/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.app

import cats.{Applicative, Monad}
import cats.data.{Chain, ReaderWriterStateT, StateT}
import cats.effect.IO
import cats.mtl.{Ask, Stateful, Tell}
import cats.syntax.all.*
import dev.nrp.puzzles.crossapix.attributes.Attributes
import dev.nrp.puzzles.crossapix.game.*
import monocle.{Focus, Lens}

/**
 * Data type for a basic [[CAPApp]] which takes no moves from the user, and lists solver moves on the command line.
 *
 * Features:
 * - Prints formatted messages about all solution steps
 * - Pauses in between steps until the user presses `Return`.
 *
 * Simple wrapper around a [[cats.data.StateT]] that wraps a [[cats.effect.IO]].
 *
 * @param run Underlying Cats monad
 * @tparam Attrs Type of the game attributes to track for the solver
 * @tparam T Type of values this monadic program returns
 */
case class CLIStratRunner[Attrs, T](run: StateT[IO, (Board, Attrs), T])

object CLIStratRunner:
  /**
   * [[CAPApp]] instance for [[CLIStratRunner]]. Basic app which takes no user moves and prints solver moves to CLI.
   *
   * Features:
   * - Prints formatted messages about all solution steps
   * - Pauses in between steps until the user presses `Return`.
   *
   * Simple wrapper around a [[cats.data.StateT]] that wraps a [[cats.effect.IO]].
   */
  given cliStratRunningApp[Attrs]: CAPApp[Attrs, CLIStratRunner] with
    // Instances from the underlying [[cats.data.ReaderWriterStateT]] to help with implementing actions.
    private val M = Monad[[T] =>> StateT[IO, (Board, Attrs), T]]
    private val S = Stateful[[T] =>> StateT[IO, (Board, Attrs), T], (Board, Attrs)]
    private val _board: Lens[(Board, Attrs), Board] = Focus[(Board, Attrs)](_._1)
    private val _attrs: Lens[(Board, Attrs), Attrs] = Focus[(Board, Attrs)](_._2)

    /** Alias for lifting the wrapped [[cats.data.StateT]] to a [[CLIStratRunner]]. */
    private def liftF[T](inner: StateT[IO, (Board, Attrs), T]): CLIStratRunner[Attrs, T] = CLIStratRunner(inner)

    /** Helper method for lifting [[cats.effect.IO]] actions into full [[CLIStratRunner]] actions. */
    private def liftIO[T](io: IO[T]): CLIStratRunner[Attrs, T] = liftF(StateT.liftF(io))

    // Monad operation implements -- these just pass to the wrapped monad.
    def pure[T](x: T): CLIStratRunner[Attrs, T] = liftF(M.pure(x))
    def flatMap[A, B](fa: CLIStratRunner[Attrs, A])(f: A => CLIStratRunner[Attrs, B]): CLIStratRunner[Attrs, B] =
      liftF(M.flatMap(fa.run)(a => f(a).run))
    def tailRecM[A, B](a: A)(f: A => CLIStratRunner[Attrs, Either[A, B]]): CLIStratRunner[Attrs, B] =
      val innerF = (a: A) => f(a).run
      liftF(M.tailRecM(a)(innerF))

    /** Retrieve the game board. */
    def getBoard: CLIStratRunner[Attrs, Board] = liftF(S.inspect(_board.get))

    /** Retrieve the solver attributes. */
    def getAttributes: CLIStratRunner[Attrs, Attrs] = liftF(S.inspect(_attrs.get))

    /** Update the solver attributes. */
    def modifyAttributes(f: Attrs => Attrs): CLIStratRunner[Attrs, Unit] =
      liftF(S.modify(_attrs.modify(f)))

    /**
     * Handle a step from the solver.
     *
     * This application:
     * - Prints a human-readable description of the move and its logic
     * - Waits for the user to press `Return`
     *
     * An example of output:
     * ```
     * Outlier: Col(64) can only have 5 more White spaces. So, any region which intersects this line in more than 5 spaces must be Black.
     * Black: Region 18 [9 spaces including (row 0, col 63)]
     * Black: Region 375 [17 spaces including (row 34, col 62)]
     * ```
     */
    def handleStepAndGetInput(step: SolutionStep): CLIStratRunner[Attrs, List[Move]] =
      def description(board: Board): IO[String] =
        step.exp
          .description
          .map {
            case LinesReference(lineIds, caption) => lineIds.map(_.toString).mkString(", ")
            case Text(s) => s
            case RegionsReference(regionIds, caption) => regionIds.map( regionId =>
              val space = board.regions(regionId).minBy { case Space(r, c) => (r, c) }
              s"region $regionId [${board.regions(regionId).length} spaces including (row ${space.row}, col ${space.col})]"
            ).mkString(", ")
          }
          .mkString
          .pure

      def printMoves(board: Board, moves: List[Move]): IO[Unit] =
        moves
          .map { case Move(regionId, st) =>
            val space = board.regions(regionId).minBy { case Space(r, c) => (r, c) }
            IO.println(f"  $st: Region $regionId [${board.regions(regionId).length} spaces including (row ${space.row}, col ${space.col})]")
          }
          .sequence_

      def explainStep(step: SolutionStep): CLIStratRunner[Attrs, Unit] =
        for {
          board <- getBoard
          d <- liftIO(description(board))
          _ <- liftIO(IO.println(f"${step.exp.stratName}: $d"))
          _ <- liftIO(printMoves(board, step.moves))
        } yield ()

      for {
        _ <- explainStep(step)
        _ <- liftIO(IO.readLine)
      } yield List.empty

    /**
     * Initialize and take any initial user moves.
     *
     * No-op for this [[CAPApp]].
     */
    def initialize: CLIStratRunner[Attrs, List[Move]] = pure(List.empty)

    /**
     * Respond to end of application.
     *
     * No-op for this [[CAPApp]].
     */
    def close: CLIStratRunner[Attrs, Unit] = pure(())

    /**
     * 'Compile' a [[CLIStratRunner]] into a [[cats.effect.IO]] application with the given board.
     */
    def run[A](app: CLIStratRunner[Attrs, A], board: Board, attrs: Attrs): IO[A] = app.run.runA(board, attrs)