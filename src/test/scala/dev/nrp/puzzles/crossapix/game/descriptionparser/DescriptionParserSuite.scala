package dev.nrp.puzzles.crossapix.game.descriptionparser

import dev.nrp.puzzles.crossapix.game.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

import scala.util.chaining.scalaUtilChainingOps

extension (sc: StringContext)
  def ps(args: Any*): ParserState = ParserState(sc.parts.toList, args.toList)

class DescriptionParserSuite extends AnyFlatSpec with Matchers {
  val D: DescriptionParser.type = DescriptionParser
  
  def eval[A](parser: DescParser[A], input: ParserState): (ParserState, Either[ParseError, A]) =
    parser.value.run(input).value

  def evalS[A](parser: DescParser[A], input: ParserState): ParserState = eval(parser, input)._1

  def evalA[A](parser: DescParser[A], input: ParserState): Either[ParseError, A] = eval(parser, input)._2

  def handleBadInput[A](parser: DescParser[A], badInput: ParserState): Unit = {
    val (s, r) = eval(parser, badInput)
    s shouldEqual badInput
    r.isLeft shouldBe true
  }

  "DescParser.|" should "use the first parser that succeeds" in {
    val good1 = D.P.parseLiteralChar(',')
    val good2 = (D.P.parseLiteralChar(',') ~ D.P.parseLiteralChar(' ')).map(_._1)
    val bad1 = D.P.parseLiteralChar(':')
    val bad2 = D.P.parseLiteralChar(';')

    val init = ps", therefore I am"

    val good1Eval = eval(good1, init)
    val good2Eval = eval(good2, init)

    eval(good1 | good2 | bad1 | bad2, init) shouldEqual good1Eval
    eval(bad1 | good1 | good2 | bad2, init) shouldEqual good1Eval
    eval(bad1 | bad2 | good1 | good2, init) shouldEqual good1Eval
    eval(good2 | good1 | bad1 | bad2, init) shouldEqual good2Eval
    eval(bad1 | good2 | good1 | bad2, init) shouldEqual good2Eval
    eval(bad1 | bad2 | good2 | good1, init) shouldEqual good2Eval
  }

  it should "fail if all parsers fail, and leave state unchanged" in {
    val bad1 = (D.P.parseLiteralChar('t') ~ D.P.parseLiteralChar('w')).map(_._1)
    val bad2 = D.P.parseLiteralChar('{')

    val init = ps"therefore I am"

    List(bad1 | bad2, bad2 | bad1).foreach(handleBadInput(_, init))
  }

  "DescParser.~" should "succeed if input parsers succeed sequentially and match effect of basic chaining" in {
    val init = ps"therefore I am"
    val p1 = D.P.parseLiteralChar('t')
    val p2 = D.P.parseLiteralChar('h')
    val p3 = D.P.parseLiteralChar('e')

    val chained = for {
      a <- p1
      b <- p2
      c <- p3
    } yield ((a, b), c)

    val actual = eval(p1 ~ p2 ~ p3, init)
    val expected = eval(chained, init)

    actual shouldEqual expected
  }

  it should "fail if any part fails, and leave state unchanged" in {
    val init = ps"therefore I am"
    val p1 = D.P.parseLiteralChar('t')
    val p2 = D.P.parseLiteralChar('h')
    val p3 = D.P.parseLiteralChar('o')
    handleBadInput(p1 ~ p2 ~ p3, init)
  }

  "DescParser.revertStateOnFail" should "operate exactly as the base parser if it succeeds" in {
    val p = for {
      t <- D.P.parseLiteralChar('t')
      h <- D.P.parseLiteralChar('h')
    } yield (t, h)

    val init = ps"therefore I am"

    eval(p.revertStateOnFail, init) shouldEqual eval(p, init)
  }

  it should "revert to the original state if the parser fails for any reason but return the same result" in {
    val p = for {
      t <- D.P.parseLiteralChar('t')
      o <- D.P.parseLiteralChar('o')
    } yield (t, o)

    val init = ps"therefore I am"

    val (sBase, rBase) = eval(p, init)
    sBase shouldNot equal (init)
    rBase.isLeft shouldBe true

    val (s, r) = eval(p.revertStateOnFail, init)
    s shouldEqual init
    r shouldEqual rBase
  }

  "DescParser.zeroOrMore" should "return an empty list and unmodified state if the parser doesn't match" in {
    val init = ps"abcdefg"
    val p = D.P.parseLiteralChar('x')
    val (s, r) = eval(p.zeroOrMore, init)
    s shouldEqual init
    r shouldEqual Right(List.empty[Char])
  }

  it should "parse as many copies as possible if it does match" in {
    val init = ps"aaaabcdefg"
    val p = D.P.parseLiteralChar('a')
    val actual = eval(p.zeroOrMore, init)
    val expected = eval(p.repeat(4), init)
    actual shouldEqual expected

  }

  "DescParser.oneOrMore" should "succeed if the parser matches at least one time, and match as many as possible" in {
    (1 until 20).foreach { i =>
      val init = ParserState(List("ab" * i + "ac"), List.empty)
      val p = (D.P.parseLiteralChar('a') ~ D.P.parseLiteralChar('b')).oneOrMore
      val (finalState, result) = eval(p, init)

      result shouldEqual Right(List.fill(i)(('a', 'b')))
      finalState.parts shouldEqual List("ac")
      finalState.args shouldEqual List.empty
    }
  }

  it should "fail if the parser matches zero times and leave state unaltered" in {
    val init = ps"acababababac"
    val p = (D.P.parseLiteralChar('a') ~ D.P.parseLiteralChar('b')).oneOrMore
    val (finalState, result) = eval(p, init)

    result.isLeft shouldBe true
    finalState shouldEqual init
  }

  "DescParser.optional" should "succeed in parsing once (and only once) if parser matches" in {
    val init = ps"aabcdef"
    val (s, r) = eval(D.P.parseLiteralChar('a').optional, init)
    s shouldEqual ps"abcdef"
    r shouldEqual Right(Some('a'))
  }

  it should "succeed, returning None and not changing state, if there is no match" in {
    val init = ps"abcdef"
    val p = D.P.parseLiteralChar('a') ~ D.P.parseLiteralChar('a')

    val (s, r) = eval(p.optional, init)
    s shouldEqual init
    r shouldEqual Right(None)
  }

  "DescParser.repeat" should "succeed and return exactly n results if the parser matches at least n times" in {
    val init = ps"aaaaaa"
    val p = D.P.parseLiteralChar('a')
    val (s, r) = eval(p.repeat(4), init)

    s shouldEqual ps"aa"
    r shouldEqual Right(List('a', 'a', 'a', 'a'))
  }

  it should "fail and leave state unchanged if there are fewer than n matches" in {
    val init = ps"aaabaaa"
    val p = D.P.parseLiteralChar('a')
    handleBadInput(p.repeat(4), init)
  }

  "parseText" should "read until { within a part, chomping and returning the correct Text" in {
    val init = ps"Outlier {l ${LineId.Row(3)} | Line in question}"
    val (s, r) = eval(D.parseText, init)

    s.args shouldEqual init.args
    s.parts shouldEqual List("{l ", " | Line in question}")
    r shouldEqual Right(Text("Outlier "))
  }

  it should "read across parts until {, chomping parts and args as needed" in {
    val init = ps"Outlier: can only be missing ${5} more ${RegionState.Black} in {l ${LineId.Row(3)} | Line in question}"
    val (s, r) = eval(D.parseText, init)

    s.parts shouldEqual List("{l ", " | Line in question}")
    s.args shouldEqual List(LineId.Row(3))
    r shouldEqual Right(Text("Outlier: can only be missing 5 more Black in "))
  }

  it should "work even when starting from an empty part" in {
    val init = ps"${5} missing at most in {l ${LineId.Row(3)} | Line in question}"
    assert(init.parts.head.isEmpty)
    val (s, r) = eval(D.parseText, init)

    s.parts shouldEqual List("{l ", " | Line in question}")
    s.args shouldEqual List(LineId.Row(3))
    r shouldEqual Right(Text("5 missing at most in "))
  }

  it should "fail and leave state unchanged if the first character starts a region reference" in {
    val init = ps"{l ${LineId.Row(3)} | Line in question} can only be missing ${5} more spaces"
    handleBadInput(D.parseText, init)
  }

  it should "fail and leave state unchanged if we're out of parts" in {
    val inits = List(
      ParserState(List.empty, List(3, "hello")),
      ParserState(List.empty, List.empty)
    )
    inits.foreach(handleBadInput(D.parseText, _))
  }

  "refSep" should "succeed if current part is only whitespace and a single comma, and consume it" in {
    val init = ps"  , ${LineId.Row(5)} | Line in question }"
    val (s, r) = eval(D.refSep, init)

    s.parts shouldEqual List(" | Line in question }")
    s.args shouldEqual List(LineId.Row(5))
    r shouldEqual Right( () )
  }

  it should "fail and leave state unchanged if any non-whitespace, non-comma character is encountered" in {
    val init = ps"  ; ${LineId.Row(5)} | Line in question }"
    handleBadInput(D.refSep, init)
  }

  it should "fail and leave state unchanged if current part is empty" in {
    val init = ps"${5} missing at most in {l ${LineId.Row(3)} | Line in question}"
    handleBadInput(D.refSep, init)
  }

  it should "fail and leave state unchanged if we are out of parts" in {
    val init = ParserState(List.empty, List.empty)
    handleBadInput(D.refSep, init)
  }

  "parseRegionsReference" should "correctly extract a simple region reference" in {
    val init = ps"{c ${5}, ${3} | Very Important Regions} and some text"
    val (s, r) = eval(D.parseRegionsReference, init)

    s.parts shouldEqual List(" and some text")
    s.args shouldEqual List.empty
    r shouldEqual Right(RegionsReference(List(5, 3), "Very Important Regions"))
  }

  it should "correct extract region references with interpolated variables in caption" in {
    val n = 2
    val init = ps"{c ${5}, ${3} | $n Very Important Regions} and some text"
    val (s, r) = eval(D.parseRegionsReference, init)

    s.parts shouldEqual List(" and some text")
    s.args shouldEqual List.empty
    r shouldEqual Right(RegionsReference(List(5, 3), "2 Very Important Regions"))
  }

  it should "fail when given text and leave state unchanged" in {
    val init = ps"therefore I am {c ${5}| caption}"
    handleBadInput(D.parseRegionsReference, init)
  }

  it should "fail when given a line reference and leave state unchanged" in {
    val init = ps"{l ${LineId.Row(5)}, ${LineId.Col(0)} | Important lines}"
    handleBadInput(D.parseRegionsReference, init)
  }

  it should "fail when at the end and leave state unchanged" in {
    val init = ParserState(List.empty, List.empty)
    handleBadInput(D.parseRegionsReference, init)
  }

  "parseLinesReference" should "correctly extract a simple line reference" in {
    val init = ps"{l ${LineId.Row(4)}, ${LineId.Col(0)} | Very Important Lines} and some text"
    val (s, r) = eval(D.parseLinesReference, init)

    s.parts shouldEqual List(" and some text")
    s.args shouldEqual List.empty
    r shouldEqual Right(LinesReference(List(LineId.Row(4), LineId.Col(0)), "Very Important Lines"))
  }

  it should "correctly parse line references with interpolated variables in the caption" in {
    val n = 2
    val init = ps"{l ${LineId.Row(4)}, ${LineId.Col(0)} | $n Very Important Lines} and some text"
    val (s, r) = eval(D.parseLinesReference, init)

    s.parts shouldEqual List(" and some text")
    s.args shouldEqual List.empty
    r shouldEqual Right(LinesReference(List(LineId.Row(4), LineId.Col(0)), "2 Very Important Lines"))
  }

  it should "fail when given text and leave state unchanged" in {
    val init = ps"therefore I am {l ${LineId.Row(4)}| caption}"
    handleBadInput(D.parseLinesReference, init)
  }

  it should "fail when given a region reference and leave state unchanged" in {
    val init = ps"{c ${5}, ${3} | Important regions}"
    handleBadInput(D.parseLinesReference, init)
  }

  it should "fail when at the end and leave state unchanged" in {
    val init = ParserState(List.empty, List.empty)
    handleBadInput(D.parseLinesReference, init)
  }
  
  
}
