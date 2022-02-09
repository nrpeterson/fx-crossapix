package dev.nrp.puzzles.crossapix.utils

object OCRUtils:
  val digits = List(
    0 -> Array2D.fromNestedSeqs(
      List(
        List(0, 0, 1, 1, 1, 0, 0),
        List(0, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(0, 1, 1, 0, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 0),
        List(0, 0, 1, 1, 1, 1, 0)
      )
    ),
    1 -> Array2D.fromNestedSeqs(
      List(
        List(0, 0, 1, 1),
        List(0, 1, 1, 1),
        List(1, 1, 1, 1),
        List(1, 1, 1, 1),
        List(1, 0, 1, 1),
        List(0, 0, 1, 1),
        List(0, 0, 1, 1),
        List(0, 0, 1, 1),
        List(0, 0, 1, 1),
        List(0, 0, 1, 1),
        List(0, 0, 1, 1),
        List(0, 0, 1, 1)
      )
    ),
    2 -> Array2D.fromNestedSeqs(
      List(
        List(0, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 0, 0, 1, 1, 1),
        List(0, 0, 0, 0, 1, 1, 1),
        List(0, 0, 0, 1, 1, 1, 0),
        List(0, 0, 0, 1, 1, 1, 0),
        List(0, 0, 1, 1, 1, 0, 0),
        List(0, 1, 1, 1, 0, 0, 0),
        List(1, 1, 1, 1, 0, 0, 0),
        List(1, 1, 1, 1, 1, 1, 1),
        List(1, 1, 1, 1, 1, 1, 1),
      )
    ),
    3 -> Array2D.fromNestedSeqs(
      List(
        List(0, 0, 1, 1, 1, 0, 0),
        List(0, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 0, 1, 1, 0),
        List(0, 1, 0, 0, 1, 1, 1),
        List(0, 0, 0, 1, 1, 1, 0),
        List(0, 0, 0, 1, 1, 1, 0),
        List(0, 0, 0, 1, 1, 1, 1),
        List(0, 0, 0, 0, 1, 1, 1),
        List(0, 1, 0, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 0)
      )
    ),
    4 -> Array2D.fromNestedSeqs(
      List(
        List(0, 0, 0, 0, 1, 1, 0),
        List(0, 0, 0, 1, 1, 1, 0),
        List(0, 0, 0, 1, 1, 1, 0),
        List(0, 0, 1, 1, 1, 1, 0),
        List(0, 0, 1, 1, 1, 1, 0),
        List(0, 1, 1, 0, 1, 1, 0),
        List(1, 1, 1, 0, 1, 1, 0),
        List(1, 1, 1, 1, 1, 1, 1),
        List(1, 1, 1, 1, 1, 1, 1),
        List(1, 1, 1, 1, 1, 1, 1),
        List(0, 0, 0, 0, 1, 1, 0),
        List(0, 0, 0, 0, 1, 1, 0)
      )
    ),
    5 -> Array2D.fromNestedSeqs(
      List(
        List(0, 1, 1, 1, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 0, 0, 0, 0),
        List(1, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 0, 1, 1, 1),
        List(0, 0, 0, 0, 1, 1, 1),
        List(0, 1, 0, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 0)
      )
    ),
    6 -> Array2D.fromNestedSeqs(
      List(
        List(0, 0, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 1),
        List(1, 1, 1, 0, 0, 0, 0),
        List(1, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 1, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 0, 1, 1),
        List(1, 1, 1, 0, 0, 1, 1),
        List(0, 1, 1, 0, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 1),
        List(0, 0, 1, 1, 1, 1, 0)
      )
    ),
    7 -> Array2D.fromNestedSeqs(
      List(
        List(1, 1, 1, 1, 1, 1, 1),
        List(1, 1, 1, 1, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 1),
        List(0, 0, 0, 0, 1, 1, 0),
        List(0, 0, 0, 1, 1, 1, 0),
        List(0, 0, 0, 1, 1, 0, 0),
        List(0, 0, 1, 1, 1, 0, 0),
        List(0, 0, 1, 1, 1, 0, 0),
        List(0, 0, 1, 1, 0, 0, 0),
        List(0, 0, 1, 1, 0, 0, 0),
        List(0, 0, 1, 1, 0, 0, 0),
        List(0, 0, 1, 1, 0, 0, 0)
      )
    ),
    8 -> Array2D.fromNestedSeqs(
      List(
        List(0, 1, 1, 1, 1, 0, 0),
        List(0, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 0, 1, 1, 0),
        List(1, 1, 1, 0, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 1, 0)
      )
    ),
    9 -> Array2D.fromNestedSeqs(
      List(
        List(0, 1, 1, 1, 1, 0, 0),
        List(1, 1, 1, 1, 1, 1, 0),
        List(1, 1, 1, 1, 1, 1, 0),
        List(1, 1, 0, 0, 1, 1, 1),
        List(1, 1, 0, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 1),
        List(1, 1, 1, 1, 1, 1, 1),
        List(0, 1, 1, 1, 1, 1, 1),
        List(0, 0, 0, 0, 1, 1, 1),
        List(1, 1, 1, 0, 1, 1, 0),
        List(1, 1, 1, 1, 1, 1, 0),
        List(0, 1, 1, 1, 1, 0, 0)
      )
    )
  )

  def findDigit(hayStack: Array2D[Int]): List[(Int, Double, (Int, Int))] =
    val base = for {
      (d, needle) <- digits
      (r, c, s) <- Array2D.tileSimilarities (needle, hayStack)
    } yield (d, s, (r, c))

    val sorted = base
      .filter { case (_, s, _) => s >= 0.5 }
      .sortBy { case (d, s, (r, c)) => (-1 * s, c, r) }

    def isCloseTo(candidate: (Int, Double, (Int, Int)))(current: (Int, Double, (Int, Int))) = (current, candidate) match
      case ((_, _, (r0, c0)), (_, _, (r1, c1))) => math.abs(r1 - r0) + math.abs(c1 - c0) < 5

    val keepers = sorted.foldLeft(List.empty[(Int, Double, (Int, Int))]) { case (cur, next) =>
      if cur.exists(isCloseTo(next)) then cur
      else next :: cur
    }

    keepers.sortBy { case (_, score, _) => (-1) * score }

  def segments[Base <: Array2D[Int]](arr: Base): List[Array2DView[Int, arr.This]] =
    if arr.cols < 10 then List(arr.slice(0, arr.rows, 0, arr.cols))
    else
//      val seg1 = arr.slice(0, arr.rows, 0, 7)
//      val seg2 = arr.slice(0, arr.rows, arr.cols - 7, arr.cols)
      val size = (arr.cols + 1) / 2
      val seg1 = arr.slice(0, arr.rows, 0, size)
      val seg2 = arr.slice(0, arr.rows, arr.cols - size, arr.cols)

      List(seg1, seg2)

  def resize(arr: Array2D[Int], nRows: Int, nCols: Int): Array2DBase[Int] =
    def value(i: Int, j: Int): Int =
      val oldXMid = (arr.rows.toDouble - 1) / 2.0
      val oldYMid = (arr.cols.toDouble - 1) / 2.0
      val newXMid = (nRows.toDouble - 1) / 2.0
      val newYMid = (nCols.toDouble - 1) / 2.0

      val xC = i.toDouble - newXMid
      val yC = j.toDouble - newYMid

      // (0,0) -> (0, 0). (nRows/2, nCols/2) -> (oldRows/2, oldCols/2)
      val xRatio = arr.rows.toDouble / nRows.toDouble
      val yRatio = arr.cols.toDouble / nCols.toDouble

      val xPC = xC * xRatio
      val yPC = yC * yRatio

      // newmid -> (0,0) -> (0,0) -> oldmid
      val xP = xPC + oldXMid
      val yP = yPC + oldYMid

      val points = for {
        xFunc <- List[Double => Int](a => Math.floor(a).toInt, a => Math.ceil(a).toInt)
        yFunc <- List[Double => Int](a => Math.floor(a).toInt, a => Math.ceil(a).toInt)
      } yield (xFunc(xP), yFunc(yP))

      // (1.2, 3.4) => .8*.6*(1, 3), .8*.4*(1, 4), .2*.6*(2, 3), .2*.4*(2, 4)
      //
      val pointSet =
        points.toSet
          .filter { case (x, y) => x >= 0 && y >= 0 && x < arr.rows && y < arr.cols }

      val (weightedSum, weight) = pointSet
        .map { case (x, y) => (arr(x, y), (1 - Math.abs(x - xP)) * (1 - Math.abs(y - yP))) }
        .foldLeft((0.0, 0.0)) { case ((sc, wc), (v, w)) => (sc + v*w, wc + w) }

      val wavg = weightedSum / weight

      if wavg - Math.floor(wavg) < 0.5 then Math.floor(wavg).toInt
      else Math.ceil(wavg).toInt

    val resultSeqs = (0 until nRows).map( i => (0 until nCols).map(j => value(i, j)))

    Array2D.fromNestedSeqs(resultSeqs)

  def extractNumber(hayStack: Array2D[Int]): Int =
    val trimmed = Array2D.trim(hayStack, 0)
    val ratio = trimmed.cols.toDouble / trimmed.rows.toDouble
//    println((trimmed.rows, trimmed.cols, ratio))
    val resized =
      if ratio < 14.0/12.0 then resize(trimmed, 12, Math.ceil(13 * ratio).toInt)
      else resize(trimmed, 12, 14)
//    println(resized.mkString())
    val segs = segments(resized)
//    segs.foreach( arr => println(arr.mkString() + "\n"))
    val digits = segs.map(findDigit)
//    println(digits)
    digits.foldLeft(0) { case (cur, d) => 10 * cur + d.head._1 }