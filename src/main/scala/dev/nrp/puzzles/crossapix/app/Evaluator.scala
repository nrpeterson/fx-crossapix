/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.app

import cats.data.StateT
import cats.effect.IO
import cats.Monad
import cats.mtl.Stateful
import cats.syntax.all.*
import dev.nrp.puzzles.crossapix.game.*
import monocle.{Focus, Lens}

case class Stats(
  unknownRegions: Int,
  unknownSpaces: Int,
  numMoves: Int,
  numSteps: Int,
  stratCounts: Map[String, Int]
)

object Stats:
  val _unknownRegions: Lens[Stats, Int] = Focus[Stats](_.unknownRegions)
  val _unknownSpaces: Lens[Stats, Int] = Focus[Stats](_.unknownSpaces)
  val _numMoves: Lens[Stats, Int] = Focus[Stats](_.numMoves)
  val _numSteps: Lens[Stats, Int] = Focus[Stats](_.numSteps)
  val _stratCounts: Lens[Stats, Map[String, Int]] = Focus[Stats](_.stratCounts)

case class Evaluator[Attrs, T](run: StateT[IO, (Board, Attrs, Stats),T])

object Evaluator:
  given evaluatorCAPApp[Attrs]: CAPApp[Attrs, Evaluator] with
    private val M = Monad[[A] =>> StateT[IO, (Board, Attrs, Stats), A]]
    private val S = Stateful[[A] =>> StateT[IO, (Board, Attrs, Stats), A], (Board, Attrs, Stats)]
    private val _board: Lens[(Board, Attrs, Stats), Board] = Focus[(Board, Attrs, Stats)](_._1)
    private val _attrs: Lens[(Board, Attrs, Stats), Attrs] = Focus[(Board, Attrs, Stats)](_._2)
    private val _stats: Lens[(Board, Attrs, Stats), Stats] = Focus[(Board, Attrs, Stats)](_._3)

    // Members declared in cats.Applicative
    def pure[A](x: A): Evaluator[Attrs, A] = Evaluator(M.pure(x))

    // Members declared in cats.FlatMap 
    def flatMap[A, B](fa: Evaluator[Attrs, A])(f: A => Evaluator[Attrs, B]): Evaluator[Attrs, B] =
      Evaluator(M.flatMap(fa.run)(a => f(a).run))
    def tailRecM[A, B](a: A)(f: A => Evaluator[Attrs, Either[A, B]]): Evaluator[Attrs, B] =
      Evaluator(M.tailRecM(a)((a0: A) => f(a).run))

    private def lift[T](inner: StateT[IO, (Board, Attrs, Stats), T]): Evaluator[Attrs, T] = Evaluator(inner)
    private def liftIO[T](action: IO[T]): Evaluator[Attrs, T] = Evaluator(StateT.liftF(action))
    private def modifyStats(f: Stats => Stats): Evaluator[Attrs, Unit] = lift(S.modify(_stats.modify(f)))
    private def getStats: Evaluator[Attrs, Stats] = lift(S.inspect(_stats.get))

    def getAttributes: Evaluator[Attrs, Attrs] = lift(S.inspect(_attrs.get))
    def modifyAttributes(f: Attrs => Attrs): Evaluator[Attrs, Unit] = lift(S.modify(_attrs.modify(f)))
    def getBoard: Evaluator[Attrs, Board] = lift(S.inspect(_board.get))
    def initialize: Evaluator[Attrs, List[Move]] = pure(List.empty)

    def handleStepAndGetInput(step: SolutionStep): Evaluator[Attrs, List[Move]] =
      def f(board: Board): Stats => Stats =
        val moves = step.moves.length
        val spaces = step.moves.map( mv => board.regions(mv.regionId).length).sum
        val strat = step.exp.stratName

        Stats._unknownRegions.modify(_ - moves)
          .compose(Stats._unknownSpaces.modify(_ - spaces))
          .compose(Stats._numMoves.modify(_ + moves))
          .compose(Stats._numSteps.modify(_ + 1))
          .compose(Stats._stratCounts.at(strat).modify { opt => Some(opt.getOrElse(0) + 1) })

      for {
        board <- getBoard
        _ <- modifyStats(f(board))
      } yield List.empty

    def close: Evaluator[Attrs, Unit] = for {
      stats <- getStats
      _ <- liftIO(IO.println(stats))
    } yield ()

    def run[T](app: Evaluator[Attrs, T], board: Board, attrs: Attrs ): IO[T] =
      val stats = Stats(board.regions.size, board.numRows * board.numCols, 0, 0, Map.empty)
      app.run.runA((board, attrs, stats))
