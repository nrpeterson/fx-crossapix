/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MapUtilsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "MapUtils.zipWith" should "contain exactly shared elements" in {
    forAll { (l: Map[Int, Int], r: Map[Int, Boolean]) =>
      val result = MapUtils.zipWith(Tuple2.apply, l, r)
      val expected = l.keySet.intersect(r.keySet)

      result.keySet shouldEqual expected
    }
  }

  "MapUtils.zip" should "contain exactly shared elements and properly tuple values" in {
    forAll { (l: Map[Int, Int], r: Map[Int, Boolean]) =>
      val result = MapUtils.zip(l, r)

      l.keySet.intersect(r.keySet).foreach { k =>
        result should contain key k
        result(k) shouldEqual (l(k), r(k))
      }

    }
  }
}
