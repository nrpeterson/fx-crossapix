package dev.nrp.puzzles.crossapix.game.descriptionparser

import cats.data.Chain
import cats.mtl.{Stateful, Tell}
import dev.nrp.puzzles.crossapix.game.*
import monocle.{Focus, Lens}
import monocle.syntax.all.*

import scala.util.chaining.scalaUtilChainingOps

class LowLevelParsers:
  val S = Stateful[DescParser, ParserState]

  val parts: Lens[ParserState, List[String]] = Focus[ParserState](_.parts)
  val args: Lens[ParserState, List[Any]] = Focus[ParserState](_.args)

  val viewPart: DescParser[String] =
    S.inspect(parts.to(_.headOption).get)
      .flatMap(fromOpt(_, "At least one part", "No remaining parts"))

  val chompPart: DescParser[String] = for {
    part <- viewPart
    _ <- S.modify(parts.modify(_.tail))
  } yield part

  val viewChar: DescParser[Char] =
    viewPart.map(_.headOption)
      .flatMap(fromOpt(_, "At least one character in current part", "An empty part"))

  val chompChar: DescParser[Char] = for {
    c <- viewChar
    _ <- S.modify( _.focus(_.parts.index(0)).modify(_.tail))
  } yield c

  val viewArg: DescParser[Any] =
    S.inspect(args.to(_.headOption).get)
      .flatMap(fromOpt(_, "At least one argument remaining", "No remaining arguments"))

  val chompArg: DescParser[Any] = for {
    a <- viewArg
    _ <- S.modify(args.modify(_.tail))
  } yield a

  val chompRegionId: DescParser[RegionId] = for {
    a <- viewArg
    cOpt = a match
      case c: RegionId => Some(c)
      case _ => None
    result <- fromOpt(cOpt, "RegionID", a.toString)
    _ <- chompArg
  } yield result

  val chompLineId: DescParser[LineId] = for {
    a <- viewArg
    cOpt = a match
      case c: LineId => Some(c)
      case _ => None
    result <- fromOpt(cOpt, "LineId", a.toString)
    _ <- chompArg
  } yield result

  def parsePartsAndArgsUntilChar(c: Char): DescParser[String] =
    for {
      part <- viewPart
      result <- {
        if part.contains(c) then
          val (pre, rest) = part.span(_ != c)
          replaceFirstPart(rest).map(_ => pre)
        else for {
          _ <- chompPart
          wasLastPart <- S.inspect(parts.to(_.isEmpty).get)
          rest <- {
            if wasLastPart then S.monad.pure("")
            else (chompArg ~ parsePartsAndArgsUntilChar(c)).map { case (a, r) => s"$a$r" }
          }
        } yield s"$part$rest"
      }
    } yield result

  def verify(p: Boolean, expected: String, actual: String): DescParser[Unit] =
    if p then result( () )
    else error(expected, actual)

  def parseLiteralChar(expected: Char): DescParser[Char] = for {
    c <- viewChar
    _ <- verify(c == expected, s"'$expected'", s"'$c'")
    _ <- chompChar
  } yield c

  val chompWhitespace: DescParser[Unit] =
    val atEnd = for {
      isEmpty <- S.inspect(parts.to(_.isEmpty).get)
      _ <- verify(isEmpty, "No remaining parts", "At least one remaining part")
    } yield ()

    val atEndOfPart = for {
      isEmpty <- S.inspect(parts.to(_.head.isEmpty).get)
      _ <- verify(isEmpty, "No remaining characters in current part", "At least one remaining character in part")
    } yield ()

    val nonTrivial = for {
      c <- viewChar
      _ <- if c.isWhitespace then chompChar.flatMap(_ => chompWhitespace) else result( () )
    } yield ()

    atEnd | atEndOfPart | nonTrivial

  val chompEndOfPart: DescParser[Unit] = for {
    p <- viewPart
    _ <- verify(p.isEmpty, "End-of-Part", p)
    _ <- chompPart
  } yield ()

  val parseEnd: DescParser[Unit] = for {
    ps <- S.inspect(parts.get)
    _ <- verify(ps.isEmpty || (ps.length == 1 && ps.head.isEmpty), "Parts to be Empty", s"${ps.length} Elements")
    as <- S.inspect(args.get)
    _ <- verify(as.isEmpty, "Args to be Empty", as.toString)
  } yield ()

  def replaceFirstPart(s: String): DescParser[Unit] = for {
    _ <- viewPart
    _ <- S.modify(parts.modify { case _ :: t => s :: t })
  } yield ()

  def modifyFirstPart(f: String => String): DescParser[Unit] = for {
    _ <- viewPart
    _ <- S.modify(parts.modify { case h :: t => f(h) :: t })
  } yield ()