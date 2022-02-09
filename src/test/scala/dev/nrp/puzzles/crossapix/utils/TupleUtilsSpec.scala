package dev.nrp.puzzles.crossapix.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

import scala.util.NotGiven

class TupleUtilsSpec extends AnyFlatSpec with Matchers {
  "Insert" should "add a type at the end if it isn't in the tuple" in {
    summon[Insert[String, (Int, Boolean)] =:= (Int, Boolean, String)]
    summon[Insert[String, EmptyTuple] =:= Tuple1[String]]
  }

  it should "not add the type if it is already present" in {
    summon[Insert[String, Tuple1[String]] =:= Tuple1[String]]
    summon[Insert[String, (String, Boolean)] =:= (String, Boolean)]
    summon[Insert[String, (Boolean, String)] =:= (Boolean, String)]
  }

  "Union" should "work!" in {
    summon[Union[(String, Boolean), (Boolean, Int)] =:= (String, Boolean, Int)]
  }

  "Find" should "work!" in {
    Find[Int, (Boolean, Int, String)]((false, 5, "hello")) shouldEqual 5
    Find[Boolean, (Boolean, Int, String)]((false, 5, "hello")) shouldEqual false
    summon[NotGiven[Find[Int, (Boolean, String)]]]
  }

  it should "select the first instance when there are multiple" in {
    Find[Int, (Int, Int, Int)]((1, 2, 3)) shouldEqual 1
  }

  "Select" should "work!" in {
    Select[(Int, String), (Boolean, String, Int)]((false, "hello", 5)) shouldEqual (5, "hello")
  }
}
