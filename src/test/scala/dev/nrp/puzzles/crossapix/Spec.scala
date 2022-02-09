/*
 * Copyright (c) 2022 Nick Peterson <nick@nrp.dev>
 */

package dev.nrp.puzzles.crossapix

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


trait Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful=100)

extension [A](gen1: Gen[A])
  def :+[B](gen2: Gen[B]): Gen[(A, B)] = for { a <- gen1; b <- gen2 } yield (a, b)
  def >>[B](gen2: A => Gen[B]): Gen[(A, B)] = for { a <- gen1; b <- gen2(a) } yield (a, b)
