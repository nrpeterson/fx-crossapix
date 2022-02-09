package dev.nrp.puzzles.crossapix.utils

import dev.nrp.puzzles.crossapix.*
import dev.nrp.puzzles.crossapix.utils.Array2D.GridSpec
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.Equality

class Array2DSpec extends Spec:
  val logicalEquality: Equality[Array2D[Int]] = (a: Array2D[Int], b: Any) => b match
    case bArr: Array2D[_] =>
      if a.rows != bArr.rows || a.cols != bArr.cols then false
      else (0 until a.rows).forall { r => (0 until a.cols).forall { c => a(r, c) == bArr(r, c) } }
    case _ => false

  type Base = Array2DBase[Int]
  type View1 = Array2DView[Int, Base]
  type View2 = Array2DView[Int, View1]

  val baseGen: Gen[Base] = for {
    rows <- Gen.choose(5, 100)
    cols <- Gen.choose(5, 100)
    data <- Gen.containerOfN[Array, Int](rows * cols, Arbitrary.arbitrary[Int])
  } yield Array2DBase(rows, cols, data)

  given array2DBaseArbitrary: Arbitrary[Base] = Arbitrary(baseGen)

  def rowGen(arr: Array2D[Int]): Gen[Int] = Gen.choose(0, arr.rows - 1)
  def colGen(arr: Array2D[Int]): Gen[Int] = Gen.choose(0, arr.cols - 1)
  def coordGen(arr: Array2D[Int]) = rowGen(arr) :+ colGen(arr)

  def badRowGen(arr: Array2D[Int]): Gen[Int] = Gen.oneOf(Gen.choose(-10000, -1), Gen.choose(arr.rows, 10000))
  def badColGen(arr: Array2D[Int]): Gen[Int] = Gen.oneOf(Gen.choose(-10000, -1), Gen.choose(arr.cols, 10000))
  def badCoordGen(arr: Array2D[Int]): Gen[(Int, Int)] = Gen.oneOf(
    rowGen(arr) :+ badColGen(arr),
    badRowGen(arr) :+ colGen(arr),
    badRowGen(arr) :+ badColGen(arr)
  )

  def viewGenFrom[B <: Array2D[Int]](arr: B): Gen[Array2DView[Int, B]] =

    for {
      rows <- Gen.pick(2, 0 until arr.rows)
      cols <- Gen.pick(2, 0 until arr.cols)
    } yield Array2DView(rows.min, rows.max, cols.min, cols.max, arr)

  def baseAndViewGen: Gen[(Base, View1)] = baseGen >> viewGenFrom

  val view1Gen: Gen[View1] = baseAndViewGen.map(_._2)

  def view1AndView2Gen: Gen[(View1, View2)] =
    (baseAndViewGen.map(_._2).suchThat { view => view.rows >= 2 && view.cols >= 2 } >> viewGenFrom)
      .suchThat { case (base, _) => base.rows >= 2 && base.cols >= 2 }

  val view2Gen: Gen[View2] = view1AndView2Gen.map(_._2)

  def arrayGen: Gen[Array2D[Int]] = Gen.oneOf(baseGen, view1Gen)

  "Array2DBase" should "validate dimensions" in {
    an [AssertionError] should be thrownBy { Array2DBase(3, 2, Array(1, 2, 3, 4, 5)) }
  }

  "Array2DBase.apply" should "throw exception for bad row/column inputs" in {

    forAll( baseGen >> badCoordGen ) { case (arr, (r, c)) =>
      whenever( r < 0 || r >= arr.rows || c < 0 || c >= arr.cols) {
        an [IndexOutOfBoundsException] should be thrownBy { arr(r, c) }
      }
    }
  }

  it should "read the underlying data in row-major order" in {
    forAll( baseGen >> coordGen ) { case (arr, (r, c)) =>
      whenever (0 <= r && r < arr.rows && 0 <= c && c < arr.cols) {
        arr(r, c) shouldEqual arr.data(r * arr.cols + c)
      }
    }
  }

  "Array2DBase.slice" should "create proper slices correctly" in {
    val array = Array2DBase(3, 2, Array(0, 1, 2, 3, 4, 5))
    val slice = array.slice(1, 3, 0, 2)
    slice shouldEqual Array2DView(1, 3, 0, 2, array)
  }

  "Array2DBase.rawCoords" should "return the coordinates unchanged" in {
   forAll(baseGen >> coordGen) { case (arr, (r, c)) =>
      whenever(0 <= r && r < arr.rows && 0 <= c && c < arr.cols) {
        arr.rawCoords(r, c) shouldEqual (r, c)
      }
    }
  }

  it should "throw exception for bad row/column inputs" in {
    forAll( baseGen >> badCoordGen ) { case (arr, (r, c)) =>
      whenever (r < 0 || r >= arr.rows || c < 0 || c >= arr.cols) {
        an [IndexOutOfBoundsException] should be thrownBy { arr.rawCoords(r, c) }
      }
    }
  }

  "Array2DView.apply" should "access values correctly" in {
    forAll( baseAndViewGen ) { case (base, view) =>
      val Array2DView(r0, r1, c0, c1, _) = view

      for {
        r <- 0 until view.rows
        c <- 0 until view.cols
      } { view(r, c) shouldEqual base(r + r0, c + c0) }
    }
  }

  "Array2DView.materialize " should "materialize to the expected Array2DBase" in {
    forAll (view1Gen) { view =>
      val mat = view.materialize
      mat shouldBe an[Array2DBase[Int]]
      mat.rows shouldEqual view.rows
      mat.cols shouldEqual view.cols

      for {
        r <- 0 until view.rows
        c <- 0 until view.cols
      } { mat(r, c) shouldEqual view(r, c) }
    }
  }

  "Array2DView.parentCoords" should "return corresponding coordinates from its immediate parent" in {
    forAll(Gen.oneOf(view1Gen, view2Gen)) { view =>
      for {
        r <- 0 until view.rows
        c <- 0 until view.cols
      } { view.parentCoords(r, c) shouldEqual (r + view.rowFrom, c + view.colFrom) }
    }
  }

  "Array2D.fromNestedSeqs" should "correctly create Array2DBase" in {
    forAll(baseGen) { arr =>
      val lists = (0 until arr.rows).toList.map { r => arr.row(r).toList }
      val result = Array2D.fromNestedSeqs(lists)
      result.rows shouldEqual lists.length
      result.cols shouldEqual lists.head.length
      for {
        r <- 0 until result.rows
        c <- 0 until result.cols
      } { result(r, c) shouldEqual lists(r)(c) }
    }
  }

  "Array2D.trim" should "find boundaries of different sizes and return an appropriate view" in {
    val base = Array2D.fromNestedSeqs(
      List(
        List(1, 1, 1, 1, 1),
        List(1, 1, 1, 3, 0),
        List(1, 1, 1, 0, 2),
        List(1, 1, 1, 1, 1),
        List(1, 1, 1, 1, 1)
      )
    )

    val view = Array2DView(1, 6, 0, 5,
      Array2D.fromNestedSeqs(
        List(
          List(1, 2, 3, 4, 5, 1),
          List(1, 1, 1, 1, 1, 1),
          List(1, 1, 1, 3, 0, 1),
          List(1, 1, 1, 0, 2, 1),
          List(1, 1, 1, 1, 1, 1),
          List(1, 1, 1, 1, 1, 1)
        )
      )
    )

    val expected = Array2D.fromNestedSeqs(
      List(
        List(3, 0),
        List(0, 2)
      )
    )

    view should equal (base) (decided by logicalEquality)

    Array2D.trim(base, 1) shouldEqual Array2DView(1, 3, 3, 5, base)
    Array2D.trim(view, 1) shouldEqual Array2DView(1, 3, 3, 5, view)

    Array2D.trim(base, 0) shouldEqual Array2DView(0, 5, 0, 5, base)
    Array2D.trim(view, 0) shouldEqual Array2DView(0, 5, 0, 5, view)
  }

  it should "return an empty array if given an array that is 'all boundary'" in {
    val base = Array2DBase(2, 3, Array(1, 1, 1, 1, 1, 1))
    val view = Array2DView(1, 3, 0, 3, Array2DBase(3, 4, Array(0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0)))
    base should equal (view) (decided by logicalEquality)

    List(base, view).foreach { arr =>
      val result = Array2D.trim(arr, 1)
      result.rows shouldEqual 0
      result.cols shouldEqual 0
    }
  }

  "Array2D.extractGrid" should "successfully extract grids and spaces!" in {
    val base = Array2D.fromNestedSeqs(
      List(
        List(0, 0, 0, 0, 0, 0, 0, 0),
        List(0, 0, 1, 0, 0, 2, 0, 0),
        List(0, 0, 1, 0, 0, 2, 0, 0),
        List(0, 0, 0, 0, 0, 0, 0, 0),
        List(0, 0, 3, 0, 0, 4, 0, 0),
        List(0, 0, 3, 0, 0, 4, 0, 0),
        List(0, 0, 0, 0, 0, 0, 0, 0)
      )
    )

    val view = Array2DView(0, 7, 0, 8, base)

    val (baseSpec, baseSpaces) = Array2D.extractGrid(base, 0)
    baseSpec.rows shouldEqual 2
    baseSpec.rowOffset shouldEqual 1
    baseSpec.rowLineSize shouldEqual 1
    baseSpec.rowRegionSize shouldEqual 2
    baseSpec.cols shouldEqual 2
    baseSpec.colOffset shouldEqual 2
    baseSpec.colLineSize shouldEqual 2
    baseSpec.colRegionSize shouldEqual 1

    baseSpaces.length shouldEqual 2
    baseSpaces.foreach { _.length shouldEqual 2 }
    baseSpaces(0)(0) shouldEqual Array2DView(1, 3, 2, 3, base)
    baseSpaces(0)(1) shouldEqual Array2DView(1, 3, 5, 6, base)
    baseSpaces(1)(0) shouldEqual Array2DView(4, 6, 2, 3, base)
    baseSpaces(1)(1) shouldEqual Array2DView(4, 6, 5, 6, base)

    val (viewSpec, viewSpaces) = Array2D.extractGrid(view, 0)
    viewSpec shouldEqual baseSpec

    viewSpaces.flatten.zip(baseSpaces.flatten).foreach { case (v, b) => v should equal (b) (decided by logicalEquality) }
  }

  it should "work when there is no outer border" in {
    val base = Array2D.fromNestedSeqs(
      List(
        List(1, 0, 0, 2),
        List(1, 0, 0, 2),
        List(0, 0, 0, 0),
        List(3, 0, 0, 4),
        List(3, 0, 0, 4)
      )
    )

    val view = Array2DView(0, 5, 0, 4, base)

    val (baseSpec, baseSpaces) = Array2D.extractGrid(base, 0)
    baseSpec shouldEqual GridSpec(2, 0, 1, 2, 2, 0, 2, 1)

    baseSpaces.length shouldEqual 2
    baseSpaces.foreach { _.length shouldEqual 2 }
    baseSpaces(0)(0) shouldEqual Array2DView(0, 2, 0, 1, base)
    baseSpaces(0)(1) shouldEqual Array2DView(0, 2, 3, 4, base)
    baseSpaces(1)(0) shouldEqual Array2DView(3, 5, 0, 1, base)
    baseSpaces(1)(1) shouldEqual Array2DView(3, 5, 3, 4, base)

    val (viewSpec, viewSpaces) = Array2D.extractGrid(view, 0)
    viewSpec shouldEqual baseSpec

    viewSpaces.flatten.zip(baseSpaces.flatten).foreach { case (v, b) => v should equal (b) (decided by logicalEquality) }
  }

  "Array2D.getXRegions" should "follow the gridspec and return appropriate views" in {
    val base = Array2D.fromNestedSeqs(
      List(
        List(0, 0, 0, 0, 0, 0, 0, 0),
        List(0, 0, 1, 0, 0, 2, 0, 0),
        List(0, 0, 1, 0, 0, 2, 0, 0),
        List(0, 0, 0, 0, 0, 0, 0, 0),
        List(0, 0, 3, 0, 0, 4, 0, 0),
        List(0, 0, 3, 0, 0, 4, 0, 0),
        List(0, 0, 0, 0, 0, 0, 0, 0)
      )
    )

    val spec = GridSpec(2, 1, 1, 2, 2, 2, 2, 1)

    val actualRowRegions = Array2D.getRowRegions(base, spec)
    val actualColRegions = Array2D.getColRegions(base, spec)

    val expectedRowRegions = List(
      Array2DView(1, 3, 0, 8, base),
      Array2DView(4, 6, 0, 8, base)
    )
    actualRowRegions shouldEqual expectedRowRegions

    val expectedColRegions = List(
      Array2DView(0, 7, 2, 3, base),
      Array2DView(0, 7, 5, 6, base)
    )
    actualColRegions shouldEqual expectedColRegions
  }

  "Array2D.mkString" should "create the expected string with the right separators" in {
    val base = Array2DBase(2, 3, Array(1, 2, 3, 4, 5, 6))
    val result = base.mkString("|", ",")
    result shouldEqual "1,2,3|4,5,6"

    val view = Array2DView(0, 2, 1, 3, base)
    view.mkString("|", ",") shouldEqual "2,3|5,6"
  }

  it should "default to newline for rows and empty space for columns" in {
    val base = Array2DBase(2, 3, Array(1, 2, 3, 4, 5, 6))
    val result = base.mkString()
    result shouldEqual "123\n456"

    val view = Array2DView(0, 2, 1, 3, base)
    view.mkString() shouldEqual "23\n56"
  }

  "Array2D.mapRows" should "correctly map rows and yield a sequence of results" in {
    val base = Array2DBase(2, 3, Array(1, 2, 3, 4, 5, 6))
    val view = Array2DView(0, 2, 1, 3, base)

    base.mapRows(_.sum) shouldEqual Seq(6, 15)

    view.mapRows(_.sum) shouldEqual Seq(5, 11)
  }

  "Array2D.mapCols" should "correct map columns and yield a sequence of results" in {
    val base = Array2DBase(2, 3, Array(1, 2, 3, 4, 5, 6))
    val view = Array2DView(0, 2, 1, 3, base)

    base.mapCols(_.sum) shouldEqual Seq(5, 7, 9)

    view.mapCols(_.sum) shouldEqual Seq(7, 9)
  }

  "Array2D.similarity" should "successfully compute the fraction of regions on which two arrays agree" in {
    val arr1 = Array2D.fromNestedSeqs(
      List(
        List(0, 0, 2, 0),
        List(0, 5, 0, 7),
        List(8, 0,10, 0)
      )
    )
    val arr2 = Array2D.fromNestedSeqs(
      List(
        List(0, 1, 2, 3),
        List(4, 5, 6, 7),
        List(8, 9,10,11)
      )
    )
    val arr3 = Array2DBase(3, 4, Array(0,0,0,0,0,0,0,0,0,0,0,0))

    Array2D.similarity(arr1, arr2) shouldEqual 0.5
    Array2D.similarity(arr1, arr3) shouldEqual (7.toDouble / 12)
    Array2D.similarity(arr2, arr3) shouldEqual (1.toDouble / 12)
  }

  it should "be symmetric" in {
    val arr1 = Array2D.fromNestedSeqs(
      List(
        List(0, 0, 2, 0),
        List(0, 5, 0, 7),
        List(8, 0,10, 0)
      )
    )
    val arr2 = Array2D.fromNestedSeqs(
      List(
        List(0, 1, 2, 3),
        List(4, 5, 6, 7),
        List(8, 9,10,11)
      )
    )
    val arr3 = Array2DBase(3, 4, Array(0,0,0,0,0,0,0,0,0,0,0,0))

    for (l <- List(arr1, arr2, arr3); r <- List(arr1, arr2, arr3)) {
      Array2D.similarity(l, r) shouldEqual Array2D.similarity(r, l)
    }
  }

  "Array2D.tileSimilarities" should "work!" in {
    val haystack = Array2D.fromNestedSeqs(
      List(
        List(0, 1, 0, 1, 0),
        List(1, 2, 1, 2, 1),
        List(0, 1, 0, 1, 0)
      )
    )

    val needle = Array2D.fromNestedSeqs(
      List(
        List(0, 1),
        List(1, 0)
      )
    )

    val actual = Array2D.tileSimilarities(needle, haystack)

    val expected = List(
      (0, 0, 0.75),
      (0, 1, 0.0),
      (0, 2, 0.75),
      (0, 3, 0.0),
      (1, 0, 0.0),
      (1, 1, 0.75),
      (1, 2, 0.0),
      (1, 3, 0.75)
    )

    actual should contain theSameElementsAs expected
  }

  "Array2D.concat" should "work" in {
    val gen = for {
      rows <- Gen.choose(1, 100)
      cols1 <- Gen.choose(1, 100)
      cols2 <- Gen.choose(1, 100)
      data1 <- Gen.containerOfN[Array, Int](rows * cols1, Arbitrary.arbitrary[Int])
      data2 <- Gen.containerOfN[Array, Int](rows * cols2, Arbitrary.arbitrary[Int])
    } yield (Array2DBase(rows, cols1, data1), Array2DBase(rows, cols2, data2))

    forAll(gen) { case (arr1, arr2) =>
      whenever(arr1.rows == arr2.rows) {
        val result = Array2D.concat(arr1, arr2)
        val view1 = result.slice(0, arr1.rows, 0, arr1.cols)
        val view2 = result.slice(0, arr2.rows, arr1.cols, arr1.cols + arr2.cols)

        val actual1 = view1.materialize
        val actual2 = view2.materialize

        actual1.rows shouldEqual arr1.rows
        actual1.cols shouldEqual arr1.cols
        actual1.data.zip(arr1.data).forall(_ == _)
        actual2.rows shouldEqual arr2.rows
        actual2.cols shouldEqual arr2.cols
        actual2.data.zip(arr2.data).forall(_ == _)
      }
    }
  }

  it should "raise an exception when the arrays have different # of rows" in {
    forAll(arrayGen :+ arrayGen) { case (l, r) =>
      whenever(l.rows != r.rows) {
        an [IllegalArgumentException] should be thrownBy { Array2D.concat(l, r) }
      }
    }
  }

