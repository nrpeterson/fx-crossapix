/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class ListUtilsSpec extends AnyFlatSpec with Matchers {
  "ListUtils.zipWithIndex2" should "succesfully embed indices in nested lists" in {
    val list = List(
      List(1, 2, 3, 4, 5),
      List(6, 7),
      List(),
      List(8, 9, 10)
    )

    val expected = List(
      ((0, 0), 1), ((0, 1), 2), ((0, 2), 3), ((0, 3), 4), ((0, 4), 5),
      ((1, 0), 6), ((1, 1), 7),
      ((3, 0), 8), ((3, 1), 9), ((3, 2), 10)
    )
    
    ListUtils.zipWithIndex2(list).toList shouldEqual expected
  }

  "ListUtils.mapWithIndex2" should "maintain the same shape as its input" in {
    val input = List(
      List(1, 2, 3),
      List(4),
      List.empty,
      List(5, 6, 7, 8)
    )

    val result = ListUtils.mapWithIndex2( (_, _, _) => 0)(input)
    result.length shouldEqual input.length
    input.zip(result).foreach { case (l, r) => r.length shouldEqual l.length }
  }

  it should "map the correct indices to values" in {
    val input = List(
      List(1, 2, 3),
      List(4),
      List.empty,
      List(5, 6, 7, 8)
    )

    val result = ListUtils.mapWithIndex2( (_, _, _) )(input)

    val expected = List(
      List((0,0,1), (0,1,2), (0,2,3)),
      List((1,0,4)),
      List.empty,
      List((3,0,5), (3,1,6), (3,2,7), (3,3,8))
    )

    result shouldEqual expected
  }

  "ListUtils.toCounts" should "contain one entry for each unique element of the list" in {
    val list = List(1, 2, 3, 1, 1, 1, 2, 3, 10, 9, -1, 5, 5, 4, 2, 1, 1)
    val result = ListUtils.toCounts(list)

    result.size shouldEqual list.distinct.length
  }

  it should "correctly count distinct copies of each value" in {
    val list = List(1, 2, 3, 1, 1, 1, 2, 3, 10, 9, -1, 5, 5, 4, 2, 1, 1)
    val result = ListUtils.toCounts(list)

    list.distinct.foreach { v =>
      result should contain key v
      result(v) shouldEqual list.filter(_ == v).length
    }
  }
}
