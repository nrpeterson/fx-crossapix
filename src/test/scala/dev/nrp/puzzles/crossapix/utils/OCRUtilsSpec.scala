/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.utils

import dev.nrp.puzzles.crossapix.*
import org.scalacheck.Gen

class OCRUtilsSpec extends Spec {
  val digitGen: Gen[(Int, Array2D[Int])] = Gen.oneOf(OCRUtils.digits)
    .map { case (d, arr) =>
      if arr.cols == 7 then (d, arr)
      else (d, padLeft(padRight(padRight(arr))))
    }

  def padLeft(arr: Array2D[Int]): Array2D[Int] =
    val pad = Array2DBase(arr.rows, 1, (0 until arr.rows).map(_ => 0).toArray)
    Array2D.concat(pad, arr)

  def padRight(arr: Array2D[Int]): Array2D[Int] =
    val pad = Array2DBase(arr.rows, 1, (0 until arr.rows).map(_ => 0).toArray)
    Array2D.concat(arr, pad)

  def padGen(arr: Array2D[Int]): Gen[Array2D[Int]] = for {
    funcs <- Gen.listOfN(7 - arr.cols, Gen.oneOf(padLeft, padRight))
  } yield funcs.foldLeft(arr) { (curr, f) => f(curr) }

  def genArrayOfSize(rows: Int, cols: Int): Gen[Array2DBase[Int]] =
    Gen.containerOfN[Array, Int](rows * cols, Gen.oneOf(0, 1))
      .map( Array2DBase(rows, cols, _) )

//  val paddedDigitGen: Gen[(Int, Array2D[Int])] = for {
//    (d, arr) <- digitGen
//    arrP <- padGen(arr)
//  } yield (d, arrP)

  val twoDigitGen: Gen[(Int, Int, Array2DBase[Int])] =
    (digitGen :+ digitGen).map { case ((d1, arr1), (d2, arr2)) => (d1, d2, Array2D.concat(arr1, arr2)) }

  "OCRUtils.findDigits" should "correctly match the archetype digits" in {
    forAll(digitGen) { case (d, arr) =>
      val results = OCRUtils.findDigit(arr)
      results.length shouldEqual 1
      results.head._1 shouldEqual d
      results.head._2 shouldEqual 1.0
    }
  }

  "OCRUtils.segments" should "return a single segment for 'small' images" in {
    val gen = Gen.choose(1, 9).flatMap(genArrayOfSize(12, _))

    forAll(gen) { arr =>
      whenever(1 <= arr.cols && arr.cols <= 9) {
        val result = OCRUtils.segments(arr)
        result.length shouldEqual 1
        result.head.hasSameValues(arr) shouldEqual true
      }
    }
  }

  it should "return two equal-sized segments for 'large' images" in {
    val gen = Gen.choose(10, 20).flatMap(genArrayOfSize(12, _))

    forAll(gen) { arr =>
      whenever(arr.cols >= 10) {
        val result = OCRUtils.segments(arr)
        result.length shouldEqual 2
        val seg1 = result.head
        val seg2 = result(1)

        seg1.rows shouldEqual arr.rows
        seg2.rows shouldEqual arr.rows
        seg1.cols shouldEqual seg2.cols
        seg1.colFrom shouldEqual 0
        seg1.colUntil shouldEqual seg1.cols
        seg2.colFrom shouldEqual (arr.cols - seg2.cols)
        seg2.colUntil shouldEqual arr.cols

        2 * seg1.cols should be >= arr.cols
        2 * seg1.cols should be <= arr.cols + 1
      }
    }
  }

  "OCRUtils.resize" should "be a no-op if the new and old dimensions match" in {
    val gen = for {
      rows <- Gen.choose(1, 100)
      cols <- Gen.choose(1, 100)
      data <- Gen.containerOfN[Array, Int](rows * cols, Gen.oneOf(0, 1))
    } yield Array2DBase(rows, cols, data)

    forAll(Gen.oneOf(gen, digitGen.map(_._2), twoDigitGen.map(_._3))) { arr =>
      val result = OCRUtils.resize(arr, arr.rows, arr.cols)
      result.hasSameValues(arr) shouldBe true
    }
  }

  "OCRUtils.extractNumber" should "work on the base digits without resizing" in {
    forAll(digitGen) { case (d, arr) =>
      OCRUtils.extractNumber(arr) shouldEqual d
    }
  }

  it should "work on pairs of the base digits without resizing" in {
    import org.scalacheck.Shrink.shrinkAny

    forAll(twoDigitGen) { case (d1, d2, arr) =>
      println((d1, d2))
      println(arr.mkString())

      OCRUtils.extractNumber(arr) shouldEqual (10*d1 + d2)
    }
  }



}
