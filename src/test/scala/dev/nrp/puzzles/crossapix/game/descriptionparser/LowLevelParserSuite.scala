package dev.nrp.puzzles.crossapix.game.descriptionparser

import dev.nrp.puzzles.crossapix.game.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class LowLevelParserSuite extends AnyFlatSpec with Matchers {

  val P = new LowLevelParsers

  def eval[A](parser: DescParser[A], input: ParserState): (ParserState, Either[ParseError, A]) =
    parser.value.run(input).value

  def evalS[A](parser: DescParser[A], input: ParserState): ParserState = eval(parser, input)._1

  def evalA[A](parser: DescParser[A], input: ParserState): Either[ParseError, A] = eval(parser, input)._2
  
  def handleBadInput[A](parser: DescParser[A], badInput: ParserState): Unit = {
      evalS(parser, badInput) shouldEqual badInput
      evalA(parser, badInput).isLeft shouldBe true
  }

  "viewPart" should "return the first part without consuming it" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "greetings"))
    val (s, r) = eval(P.viewPart, init)

    s shouldEqual init
    r shouldEqual Right("hello")
  }

  it should "fail and return state unmodified if no parts remain" in {
    val init = ParserState(List.empty, List(3, "greetings"))
    val (s, r) = eval(P.viewPart, init)

    s shouldEqual init
    r.isLeft shouldBe true
  }

  "chompPart" should "consume and return the first part" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "greetings"))
    val (s, r) = eval(P.chompPart, init)

    s.args shouldEqual init.args
    s.parts shouldEqual init.parts.tail
    r shouldEqual Right("hello")
  }

  it should "fail and leave state unchanged if parts is empty" in {
    handleBadInput(P.chompPart, ParserState(List.empty, List(3, "good")))
  }

  "viewChar" should "return the first char without consuming it" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "greetings"))
    val (s, r) = eval(P.viewChar, init)

    s shouldEqual init
    r shouldEqual Right('h')
  }

  it should "fail and return state unmodified if no parts remain" in {
    handleBadInput(P.viewChar, ParserState(List.empty, List(3, "greetings")))
  }

  "chompChar" should "consume the first char and return it" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "greetings"))
    val (s, r) = eval(P.chompChar, init)

    s.args shouldEqual init.args
    s.parts.head shouldEqual "ello"
    s.parts.tail shouldEqual init.parts.tail
    r shouldEqual Right('h')
  }

  it should "fail and return state unmodified if no parts remain" in {
    handleBadInput(P.chompChar, ParserState(List.empty, List(3, "greetings")))
  }

  "chompArg" should "consume and return the first argument" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "greetings"))
    val (s, r) = eval(P.chompArg, init)

    s.args shouldEqual init.args.tail
    s.parts shouldEqual init.parts
    r shouldEqual Right(3)
  }

  it should "fail return state unmodified if no args remain" in {
    handleBadInput(P.chompArg, ParserState(List("hello", "goodbye"), List.empty))
  }

  "chompRegionId" should "consume and return the first argument if it is a RegionId" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "greetings"))
    val (s, r) = eval(P.chompRegionId, init)

    s.args shouldEqual init.args.tail
    s.parts shouldEqual init.parts
    r shouldEqual Right(3)
  }

  it should "fail if the first argument is not a RegionId and leave state unmodified" in {
    handleBadInput(P.chompRegionId, ParserState(List("hello", "goodbye"), List(LineId.Row(5), "greetings")))
  }

  it should "fail if no arguments remain and leave state alone" in {
    handleBadInput(P.chompRegionId, ParserState(List("hello", "goodbye"), List.empty))
  }

  "chompLineId" should "consume and return the first argument if it is a LineId" in {
    val init = ParserState(List("hello", "goodbye"), List(LineId.Row(5), "greetings"))
    val (s, r) = eval(P.chompLineId, init)

    s.args shouldEqual init.args.tail
    s.parts shouldEqual init.parts
    r shouldEqual Right(LineId.Row(5))
  }

  it should "fail if the first argument is not a LineId and leave state unmodified" in {
    handleBadInput(P.chompLineId, ParserState(List("hello", "goodbye"), List(3, "greetings")))
  }

  it should "fail if no arguments remain and leave state alone" in {
    handleBadInput(P.chompLineId, ParserState(List("hello", "goodbye"), List.empty))
  }

  "parsePartsAndArgsUntilChar" should "parse parts and args until the specified character" in {
    val init = ParserState(List("Outlier in ", " { ", " | Test}"), List("row", LineId.Row(5)))
    val (s, r) = eval(P.parsePartsAndArgsUntilChar('{'), init)

    s.parts shouldEqual List("{ ", " | Test}")
    s.args shouldEqual List(LineId.Row(5))
    r shouldEqual Right("Outlier in row ")
  }

  it should "consume everything if the character never occurs" in {
    val init = ParserState(List("The number of the day is ", " and I think it is ", "."), List(3, "grand"))
    val (s, r) = eval(P.parsePartsAndArgsUntilChar('{'), init)

    r shouldEqual Right("The number of the day is 3 and I think it is grand.")
    s.parts shouldEqual List.empty
    s.args shouldEqual List.empty
  }

  it should "fail if there are no parts left" in {
    handleBadInput(P.parsePartsAndArgsUntilChar('{'), ParserState(List.empty, List.empty))
  }

  "parseLiteralChar" should "succeed if the next character matches and consume/return it" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "grand"))
    val (s, r) = eval(P.parseLiteralChar('h'), init)
    r shouldEqual Right('h')
    s.args shouldEqual init.args
    s.parts.head shouldEqual "ello"
    s.parts.tail shouldEqual init.parts.tail
  }

  it should "fail and leave state unchanged if the next character doesn't match" in {
    handleBadInput(P.parseLiteralChar('{'), ParserState(List.empty, List.empty))
  }

  it should "fail and leave state unchanged if at the end of the current part, even if next part matches" in {
    handleBadInput(P.parseLiteralChar('{'), ParserState(List("", "{oodbye"), List(3, "grand")))
  }

  it should "fail and leave state unchanged if there are no remaining parts" in {
    handleBadInput(P.parseLiteralChar('{'), ParserState(List.empty, List(3, "grand")))
  }

  "chompWhitespace" should "chomp whitespace" in {
    val init = ParserState(List("  \n \thello", "goodbye"), List(3, "grand"))
    val (s, r) = eval(P.chompWhitespace, init)
    r.isRight shouldEqual true
    s.args shouldEqual init.args
    s.parts.head shouldEqual "hello"
    s.parts.tail shouldEqual init.parts.tail
  }

  it should "not cross part boundaries" in {
    val init = ParserState(List("  \n \t", "    goodbye"), List(3, "grand"))
    val (s, r) = eval(P.chompWhitespace, init)
    r.isRight shouldEqual true
    s.args shouldEqual init.args
    s.parts.head shouldEqual ""
    s.parts.tail shouldEqual init.parts.tail
  }

  it should "succeed and change nothing if there is no whitespace at front of part" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "grand"))
    val (s, r) = eval(P.chompWhitespace, init)
    r.isRight shouldEqual true
    s shouldEqual init
  }

  it should "succeed and change nothing if the current part is empty" in {
    val init = ParserState(List("", "goodbye"), List(3, "grand"))
    val (s, r) = eval(P.chompWhitespace, init)
    r.isRight shouldEqual true
    s shouldEqual init
  }

  it should "succeed and change nothing if there are no remaining parts" in {
    val init = ParserState(List.empty, List(3, "grand"))
    val (s, r) = eval(P.chompWhitespace, init)
    r.isRight shouldEqual true
    s shouldEqual init
  }

  "chompEndOfPart" should "succeed if the current part is empty and remove it" in {
    val init = ParserState(List("", "hello", "goodbye"), List(3, "grand"))
    val (s, r) = eval(P.chompEndOfPart, init)
    s.args shouldEqual init.args
    s.parts shouldEqual init.parts.tail
    r.isRight shouldEqual true
  }

  it should "fail if the current part is non-empty and leave state unchanged" in {
    handleBadInput(P.chompEndOfPart, ParserState(List("hello", "goodbye"), List(3, "grand")))
  }

  it should "fail if there are no parts left" in {
    handleBadInput(P.chompEndOfPart, ParserState(List.empty, List(3, "grand")))
  }

  "parseEnd" should "succeed and leave state unchanged if both parts and args are empty" in {
    val init = ParserState(List.empty, List.empty)
    val (s, r) = eval(P.parseEnd, init)
    s shouldEqual init
    r.isRight shouldEqual true
  }

  it should "fail and keep state unchanged if parts is empty" in {
    handleBadInput(P.parseEnd, ParserState(List.empty, List(3, "good")))
  }

  it should "fail and keep state unchanged if args is empty" in {
    handleBadInput(P.parseEnd, ParserState(List("hello", "goodbye"), List.empty))
  }

  "replaceFirstPart" should "successfully replace the first element of parts" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "good"))
    val (s, r) = eval(P.replaceFirstPart("meh"), init)
    s.args shouldEqual init.args
    s.parts shouldEqual ("meh" :: init.parts.tail)
    r shouldEqual Right(())
  }

  it should "fail if parts is empty and leave state unchanged" in {
    handleBadInput(P.replaceFirstPart("meh"), ParserState(List.empty, List(3, "good")))
  }

  "modifyFirstPart" should "successfully modify the first element of parts" in {
    val init = ParserState(List("hello", "goodbye"), List(3, "good"))
    val (s, r) = eval(P.modifyFirstPart(_.toUpperCase), init)
    s.args shouldEqual init.args
    s.parts shouldEqual ("HELLO" :: init.parts.tail)
    r.isRight shouldEqual true
  }

  it should "fail if parts is empty and leave state unchanged" in {
    handleBadInput(P.modifyFirstPart(_.toUpperCase), ParserState(List.empty, List(3, "good")))
  }
}
