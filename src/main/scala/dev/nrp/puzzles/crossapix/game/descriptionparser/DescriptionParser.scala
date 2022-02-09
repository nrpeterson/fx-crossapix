package dev.nrp.puzzles.crossapix.game.descriptionparser

import cats.data.{Chain, EitherT, ReaderWriterState, State}
import cats.mtl.{Stateful, Tell}
import cats.syntax.all.*
import cats.Monad
import dev.nrp.puzzles.crossapix.game.*
import monocle.{Focus, Lens}
import monocle.syntax.all.*

import scala.util.chaining.scalaUtilChainingOps

/**
 * State for a [[DescParser]] invoked from a [[scala.StringContext]].
 *
 * @param parts Literal string portions of the string interpolator
 * @param args Arguments to the string interpolator. Should be `RegionId` or `LineId` when inside a reference,
 *             or general string interpolation arguments when inside a reference caption or otherwise.
 */
case class ParserState(parts: List[String], args: List[Any])

/**
 * An error encountered while parsing a [[Description]] using the string interpolator.
 * 
 * @param expected String describing what we expected to find
 * @param found String describing what we found
 */
case class ParseError(expected: String, found: String):
  override def toString: String = s"Expected: $expected; Found: $found"

type ParserInternal[A] = State[ParserState, A]

/** Type for a [[Description]] parser, which is stateful and yields a result or a [[ParseError]]. */
type DescParser[A] = EitherT[ParserInternal, ParseError, A]

def error[A](expected: String, actual: String): DescParser[A] = EitherT.leftT(ParseError(expected, actual))
def result[A](value: A): DescParser[A] = EitherT.rightT(value)
def fromOpt[A](value: Option[A], expected: String, found: String): DescParser[A] =
  EitherT.fromOption(value, ParseError(expected, found))

extension [A](p1: DescParser[A])
  def |(p2: DescParser[A]): DescParser[A] = EitherT[ParserInternal, ParseError, A] {
    val S = Stateful[ParserInternal, ParserState]

    for {
      initState <- S.get
      result1 <- p1.value
      result <- {
        result1 match
          case r @ Right(_) => S.monad.pure(r)
          case Left(ParseError(ex1, f1)) => for {
            _ <- S.set (initState)
            result2 <- p2.value
            result <- {
              result2 match
                case r@Right(_) => S.monad.pure(r)
                case Left(ParseError(ex2, f2)) => S.set(initState).map(_ => Left(ParseError(s"$ex1 or $ex2", f1)))
            }
          } yield result
      }
    } yield result
  }

  def ~[B](p2: DescParser[B]): DescParser[(A, B)] =
    val attempt = for (a <- p1; b <- p2) yield (a, b)
    attempt.revertStateOnFail

  def revertStateOnFail: DescParser[A] = EitherT[ParserInternal, ParseError, A] {
    val S = Stateful[ParserInternal, ParserState]
    for {
      state <- S.get
      result <- p1.value
      _ <- if result.isLeft then S.set(state) else S.monad.pure(())
    } yield result
  }

  def zeroOrMore: DescParser[List[A]] =
    val M = Monad[DescParser]
    for {
      opt <- p1.optional
      result <- opt match
        case Some(value) => p1.zeroOrMore.map( value :: _ )
        case None => M.pure(List.empty)
    } yield result

  def oneOrMore: DescParser[List[A]] = (p1 ~ p1.zeroOrMore).map { case (h, t) => h :: t }
  def optional: DescParser[Option[A]] = p1.map(Some(_)) | result(None)
  def repeat(n: Int): DescParser[List[A]] = (0 until n).toList.map(_ => p1).sequence.revertStateOnFail

  def oneOrMoreSepBy(sep: DescParser[Unit]): DescParser[List[A]] =
    val stepper = (sep ~ p1).map( _._2 )
    (p1 ~ stepper.zeroOrMore).map { case (h, t) => h :: t }

object DescriptionParser:
  val P = new LowLevelParsers

  val parseText: DescParser[Description] =
    val curCharIsNotBrace = for {
      c <- P.viewChar
      _ <- P.verify(c != '{', "Any non-'{' character", "{")
    } yield ()

    val atEndOfPart = for {
      part <- P.viewPart
      _ <- P.verify(part.isEmpty, "Empty part", part)
    } yield ()
    for {
      _ <- atEndOfPart | curCharIsNotBrace
      text <- P.parsePartsAndArgsUntilChar('{')
    } yield Text(text)

  val refSep: DescParser[Unit] =
    (P.chompWhitespace ~ P.parseLiteralChar(',') ~ P.chompWhitespace ~ P.chompEndOfPart).map(_ => ())

  val parseRegionsReference: DescParser[Description] =
    val base: DescParser[Description] = for {
      _ <- P.parseLiteralChar('{')
      _ <- P.parseLiteralChar('c')
      part <- P.viewPart
      _ <- P.verify(part.trim.isEmpty, "Optional emptyspace then end-of-part", part)
      _ <- P.chompPart
      regions <- P.chompRegionId.oneOrMoreSepBy(refSep)
      _ <- P.chompWhitespace
      _ <- P.parseLiteralChar('|')
      _ <- P.chompWhitespace
      caption <- P.parsePartsAndArgsUntilChar('}')
      _ <- P.parseLiteralChar('}')
    } yield RegionsReference(regions, caption)

    base.revertStateOnFail

  val parseLinesReference: DescParser[Description] =
    val base: DescParser[Description] = for {
      _ <- P.parseLiteralChar('{')
      _ <- P.parseLiteralChar('l')
      part <- P.viewPart
      _ <- P.verify(part.trim.isEmpty, "Optional emptyspace then end-of-part", part)
      _ <- P.chompPart
      lines <- P.chompLineId.oneOrMoreSepBy(refSep)
      _ <- P.chompWhitespace
      _ <- P.parseLiteralChar('|')
      _ <- P.chompWhitespace
      caption <- P.parsePartsAndArgsUntilChar('}')
      _ <- P.parseLiteralChar('}')
    } yield LinesReference(lines, caption)

    base.revertStateOnFail

  val parseReference: DescParser[Description] = for {
    part <- P.viewPart
    _ <- P.verify(part(0) == '{' && Set('l', 'c').contains(part(1)), "'{c...' or '{l...'", part)
    result <- if part(1) == 'c' then parseRegionsReference else parseLinesReference
  } yield result

  val parseDescription: DescParser[Description] =
    P.viewChar.flatMap {
      case '{' => parseReference
      case _ => parseText
    }

  def run[T](parser: DescParser[T])(parts: List[String], args: List[Any]): (ParserState, Either[ParseError, T]) =
    parser
      .value
      .run(ParserState(parts, args))
      .value

  def apply(parts: List[String], args: Any*): List[Description] =
    val (_, result) =
      val parser = (parseDescription.oneOrMore ~ P.parseEnd).map(_._1)
      run(parser)(parts, args.toList)
    result match
      case Right(ds) => ds
      case Left(err) => throw Exception(err.toString)

  



