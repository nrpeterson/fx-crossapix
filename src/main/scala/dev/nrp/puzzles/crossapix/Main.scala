package dev.nrp.puzzles.crossapix

import java.io.File

import cats.{Id, Monad, Monoid}
import cats.data.*
import cats.effect.{ExitCode, IO, IOApp}
import dev.nrp.puzzles.crossapix.app.{CAPApp, CLIStratRunner, Evaluator}
import dev.nrp.puzzles.crossapix.attributes.*
import dev.nrp.puzzles.crossapix.game.*
import dev.nrp.puzzles.crossapix.heuristics.*
import dev.nrp.puzzles.crossapix.utils.*
import dev.nrp.puzzles.crossapix.vision.BoardReader

import scala.util.NotGiven

type FullH = (SaturatedLines, Outliers, Parity, PossibleLineCombos)

object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] = args.headOption match
    case Some(path) => for {
      board <- BoardReader.parseBoardFromFile(new File(path))
      _ <- CAPApp.runApp[FullH, CLIStratRunner](board)
    } yield ExitCode.Success
    case None => IO(System.err.println("Please provide path")).as(ExitCode(2))
