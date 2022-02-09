package dev.nrp.puzzles.crossapix.utils

import monocle.syntax.all.*

import scala.reflect.ClassTag

/**
 * 2-dimensional array.
 *
 * There are two implementations of this trait:
 * - `Array2DBase`, which is a simple wrapper on top of an `Array`, and
 * - `Array2DView`, which represents zooming in to a set of rectangular coordinates within a larger parent array.
 *
 * This is designed for use in [[dev.nrp.puzzles.crossapix.vision.BoardReader]] methods, both to make
 * working with the image data feel a little more 'natural' in Scala and to allow significant operation on small 
 * sections of an image without constantly making new copies of data.
 *
 * @tparam T Type of data represented in the array
 */
sealed trait Array2D[T : ClassTag]:
  /**
   * Type alias for the type of the current array.
   *
   * This allows safely moving back from views to their parent arrays, as the typesystem knows how deeply nested we are.
   */
  type This <: Array2D[T]

  /** Number of rows in the array. */
  val rows: Int

  /** Number of columns in the array. */
  val cols: Int

  /** Access the value at row r, column c in the array. Note that both row and column are 0-indexed. */
  def apply(r: Int, c: Int): T

  def hasSameValues(that: Array2D[T]): Boolean =
    if this.rows != that.rows || this.cols != that.cols then false
    else 
      val indices = for { r <- 0 until this.rows; c <- 0 until this.cols } yield (r, c)
      indices.forall( i => this.apply.tupled(i) == that.apply.tupled(i) )
  
  /** Zoom in on the sub-array at rows r0, r0+1, ... r1-1 and cols c0, c0+1, ..., c1-1. */
  def slice(r0: Int, r1: Int, c0: Int, c1: Int): Array2DView[T, This]

  /**
   * Return the coords in the `Array2DBase` in this array's lineage corresponding to (r, c) in the current array.
   *
   * If this is an `Array2DBase`, this is a no-op.  Otherwise, coordinates are translated through each `Array2DView`
   * in the lineage until a base is reached.
   *
   * @param r Row in the current array
   * @param c Column in the current array
   * @return `(r0, c0)` such that `this(r, c) == base(r0, c0)`.
   */
  def rawCoords(r: Int, c: Int): (Int, Int)

  /** If this array is a view, return an `Array2DBase` representing the same data. */
  def materialize: Array2DBase[T]

  /** Find the first row (i.e. lowest index) that satisfies the given predicate. */
  def findFirstRow(p: Seq[T] => Boolean): Option[Int] = (0 until rows).find(r => p(row(r)))

  /** Find the last row (i.e. highest index) that satisfies the given predicate. */
  def findLastRow(p: Seq[T] => Boolean): Option[Int] = (0 until rows).findLast(r => p(row(r)))

  /** Find the first column (i.e. lowest index) that satisfies the given predicate. */
  def findFirstCol(p: Seq[T] => Boolean): Option[Int] = (0 until cols).find(c => p(col(c)))

  /** Find the last column (i.e. highest index) that satisfies the given predicate. */
  def findLastCol(p: Seq[T] => Boolean): Option[Int] = (0 until cols).findLast(c => p(col(c)))

  /** Return a `Seq` representing the values in row `r` (in column order). */
  def row(r: Int): Seq[T] = (0 until cols).map(apply(r, _))

  /** Return a `Seq` representing the values in column `c` (in row order). */
  def col(c: Int): Seq[T] = (0 until rows).map(apply(_, c))

  /** View this array as a sequence of rows, and apply the given mapper to each row. */
  def mapRows[O](f: Seq[T] => O): Seq[O] = (0 until rows).map(row).map(f)

  /** View this array as a sequence of columns, and apply the given mapper to each column. */
  def mapCols[O](f: Seq[T] => O): Seq[O] = (0 until cols).map(col).map(f)

  /**
   * Create a string representation of this array.
   *
   * Uses the default `toString` method on each value, using `colSep` to join the values of each row into a string
   * and then combining those row strings with `rowSep` to get the final result.
   *
   * @param rowSep String to use to separate the row strings.  By default: newline.
   * @param colSep String to use to separate the value strings in a given row.  By default: empty string.
   * @return String representation of this array.
   */
  def mkString(rowSep: String = "\n", colSep: String = ""): String =
    (0 until rows).map(row)
      .map(_.mkString(colSep))
      .mkString(rowSep)

/**
 * `Array2D` that is a simple wrapper around an underlying data `Array`.
 *
 * The elements of the data array are assumed to be in row-major order [ r0c0, r0c1, ..., r1c0, r1c1, ...]
 *
 * @param rows Number of rows
 * @param cols Number of columns
 * @param data Underlying data `Array`. Note that it must be of length `rows * cols`
 * @tparam T Type of data represented in the array. Must have a `ClassTag` instance for storing the underlying array.
 */
case class Array2DBase[T : ClassTag](rows: Int, cols: Int, data: Array[T]) extends Array2D[T]:
  type This = Array2DBase[T]

  assert(rows * cols == data.length, "Data array must have length rows * cols")

  private def index(r: Int, c: Int): Int =
    if r < 0 || r >= rows || c < 0 || c >= cols
      then throw new IndexOutOfBoundsException(f"(r, c)=($r,$c) is out of range with $rows rows and $cols columns")
    r * cols + c

  def apply(r: Int, c: Int): T = data(index(r, c))

  def slice(r0: Int, r1: Int, c0: Int, c1: Int): Array2DView[T, This] = Array2DView(r0, r1, c0, c1, this)

  def rawCoords(r: Int, c: Int): (Int, Int) =
    if r < 0 || r >= rows || c < 0 || c >= cols
      then throw new IndexOutOfBoundsException(f"(r, c)=($r,$c) is out of range with $rows rows and $cols columns")
    (r, c)

  def materialize: Array2DBase[T] = this

/**
 * `Array2D` that is a view into a rectangular region of a parent `Array2D`.
 *
 * Represents the rectangular region with rows `rowFrom until rowTo` and columns `colFrom until colTo` -- that is,
 * `rowTo` and `colTo` are the first row and column index in the parent that are NOT included in this array.
 *
 * Simply translates a call for position `(r, c)`` in this array to a call for position `(r + rowFrom, c + colFrom)` in
 * the parent array.
 *
 * Note that views can be nested: the parent might well be a view which translates to its parent, etc.  But the lineage
 * always ends with an `Array2DBase` corresponding to the original data.
 *
 * Also note that `Array2DView` carries a specific subtype (`Base`) of `Array2D` to refer to its parent; this
 * means we can know at compile time whether the parent is an `Array2DBase` or `Array2DView`, and so statically
 * verify whether or not we are allowed to call the `parentCoords` method.
 *
 * @param rowFrom First row index of parent to include
 * @param rowUntil First row index of parent to NOT include
 * @param colFrom First column index of parent to include
 * @param colUntil First column index of parent NOT to include
 * @param base Parent `Array2D`, which may be an `Array2DBase` or an `Array2DView`.
 * @tparam T Type of data represented in the array. Must carry a `ClassTag` instance for the underlying array.
 * @tparam Base The specific subtype of `Array2D` used for the parent array.
 */
case class Array2DView[T : ClassTag, Base <: Array2D[T]](rowFrom: Int, rowUntil: Int, colFrom: Int, colUntil: Int, base: Base) extends Array2D[T]:
  type This = Array2DView[T, Base]
  val rows: Int = rowUntil - rowFrom
  val cols: Int = colUntil - colFrom

  def apply(r: Int, c: Int): T = base(r + rowFrom, c + colFrom)

  def slice(r0: Int, r1: Int, c0: Int, c1: Int): Array2DView[T, This] =

    Array2DView(r0, r1, c0, c1, this)

  def materialize: Array2DBase[T] =
    val data = for { r <- 0 until rows; c <- 0 until cols } yield apply(r, c)
    Array2DBase(rows, cols, data.toArray)

  /** Coordinates in the parent array that correspond to `(r, c)` in this array. */
  def parentCoords(r: Int, c: Int): (Int, Int) = (r + rowFrom, c + colFrom)

  def rawCoords(r: Int, c: Int): (Int, Int) = base.rawCoords(r + rowFrom, c + colFrom)

object Array2D:
  /** Create an `Array2DBase` from a nested `Seq` representation (sequence of rows). */
  def fromNestedSeqs[T : ClassTag](seqs: Seq[Seq[T]]): Array2DBase[T] =
    if seqs.isEmpty then Array2DBase(0, 0, Array.empty)
    else
      val rows = seqs.size
      val cols = seqs.head.size
      assert(seqs.forall(_.size == cols))
      val data = seqs.flatten.toArray
      Array2DBase(rows, cols, data)

  /** Create an `Array2DView` that throws away any border rows/cols which are constant with value `value`. */
  def trim[T, Base <: Array2D[T]](arr: Base, value: T): Array2DView[T, arr.This] =
    val topAndBottom = for {
      top <- arr.findFirstRow(_.exists(_ != value))
      bottom <- arr.findLastRow(_.exists(_ != value))
    } yield (top, bottom + 1)

    val (top, bottom) = topAndBottom.getOrElse((0,0))

    val leftAndRight = for {
      left <- arr.findFirstCol(_.exists(_ != value))
      right <- arr.findLastCol(_.exists(_ != value))
    } yield (left, right + 1)
    val (left, right) = leftAndRight.getOrElse((0, 0))

    arr.slice(top, bottom, left, right)

  /**
   * Specification for a grid within an `Array2D`, consisting of regions and line boundaries.
   *
   * @param rows The number of regions in each column of the grid
   * @param rowOffset Offset from the edge of the `Array2D` to the first row that is part of a region
   * @param rowLineSize Thickness of row lines
   * @param rowRegionSize Height of regions
   * @param cols The number of regions in each row of the grid
   * @param colOffset Offset from the edge of the `Array2D` to the first column that is part of a region
   * @param colLineSize Thickness of column lines
   * @param colRegionSize Width of regions
   */
  case class GridSpec(
    rows: Int,
    rowOffset: Int,
    rowLineSize: Int,
    rowRegionSize: Int,
    cols: Int,
    colOffset: Int,
    colLineSize: Int,
    colRegionSize: Int
  )

  /**
   * Extract parameters for a grid delineated by a constant value, as well as array views for each grid space.
   *
   * This is not intended for locating a grid within a larger image, but rather for processing a grid that has already
   * been located. The array should consist of nothing but the grid (including, optionally, a border).
   *
   * @param arr `Array2D` representing a grid with `borderValue`` as the marker for lines
   * @param borderValue Value that should be used for spaces representing boundaries or lines
   * @tparam T Type for the contents of the array
   * @tparam Base Specific subtype (`Array2D#This`) of `Array2D` passed in for `arr`
   * @return A `GridSpec` instance describing the layout of the grid, and a nested list (in row-major order) which
   *         contains `Array2DView` instances that zoom in on the spaces of the grid.  Note that the border is NOT
   *         included in these spaces, only the part inside the border.
   */
  def extractGrid[T, Base <: Array2D[T]](arr: Base, borderValue: T): (GridSpec, List[List[Array2DView[T, arr.This]]]) =
    // To allow for rounded corners, we allow a little fudge factor on determining whether a given row/col is a line.
    // TODO this is hacky and needs to be more robust
    val fudgeR = Math.floor(0.006 * arr.rows.toDouble).toInt
    val fudgeC = Math.floor(0.006 * arr.cols.toDouble).toInt
//    println("INSIDE EXTRACT")
//    println(arr.mkString())
//    println("FUDGE")
//    println((fudgeR, fudgeC))
//    println((arr.rows, arr.cols))
    val rowIsLine = arr.mapRows(_.drop(fudgeC).dropRight(fudgeC).forall(_ == borderValue))
    val rowStripes = ListUtils.compress(rowIsLine)

    val colIsLine = arr.mapCols(_.drop(fudgeR).dropRight(fudgeR).forall(_ == borderValue))
    val colStripes = ListUtils.compress(colIsLine)

    def lineAndRegionSize(stripes: List[(Boolean, Int)]): (Int, Int) =
      val internalStripes =
        if stripes.last._1 then stripes.dropWhile(_._1).dropRight(1)
        else stripes.dropWhile(_._1)

      val groupSizes = internalStripes
        .grouped(2)
        .filter(_.size == 2)
        .map { list => list.map(_._2).sum }
        .toSet

      val groupSize = groupSizes.sum / groupSizes.size
      assert(groupSize * groupSizes.size == groupSizes.sum)
      val lineSize = stripes.filter(_._1).map(_._2).min
      val regionSize = groupSize - lineSize
      (lineSize, regionSize)


    val rowOffset = if rowStripes.head._1 then rowStripes.head._2 else 0
    val colOffset = if colStripes.head._1 then colStripes.head._2 else 0
    val rows = rowStripes.count(!_._1)
    val (rowLineSize, rowRegionSize) = lineAndRegionSize(rowStripes)
    val (colLineSize, colRegionSize) = lineAndRegionSize(colStripes)
    val cols = colStripes.count(!_._1)

    def bounds(r: Int, c: Int): (Int, Int, Int, Int) =
      val top = rowOffset + (r * (rowLineSize + rowRegionSize))
      val bottom = top + rowRegionSize
      val left = colOffset + (c * (colLineSize + colRegionSize))
      val right = left + colRegionSize
      (top, bottom, left, right)

    val regions = (0 until rows).toList.map { r =>
      (0 until cols).toList.map { c => arr.slice.tupled(bounds(r, c)) }
    }
    val gridSpec = GridSpec(rows, rowOffset, rowLineSize, rowRegionSize, cols, colOffset, colLineSize, colRegionSize)

    (gridSpec, regions)

  /** Given a specification for a grid, return `Array2DView`s that zoom in to the rows of the grid. */
  def getRowRegions[T, Base <: Array2D[T]](arr: Base, gridSpec: GridSpec): List[Array2DView[T, arr.This]] =
    val GridSpec(rows, rowOffset, rowLineSize, rowRegionSize, _, _, _, _) = gridSpec
    (0 until rows).toList
      .map { r =>
        val start = rowOffset + r * (rowLineSize + rowRegionSize)
        val end = start + rowRegionSize
        arr.slice(start, end, 0, arr.cols)
      }

  /** Given a specification for a grid, return `Array2DView`s that zoom in to the columns of the grid. */
  def getColRegions[T, Base <: Array2D[T]](arr: Base, gridSpec: GridSpec): List[Array2DView[T, arr.This]] =
    val GridSpec(_, _, _, _, cols, colOffset, colLineSize, colRegionSize) = gridSpec
    (0 until cols).toList
      .map { c =>
        val start = colOffset + c * (colLineSize + colRegionSize)
        val end = start + colRegionSize
        arr.slice(0, arr.rows, start, end)
      }

  /** Return the fraction of spaces on which two arrays of the same shape agree. */
  def similarity[T](a: Array2D[T], b: Array2D[T]): Double =
    assert(a.rows == b.rows && a.cols == b.cols, "Arrays must have same dimensions")
    val indices = for { r <- 0 until a.rows; c <- 0 until a.cols } yield (r, c)
    val numAgree = indices.count { case (r, c) => a(r, c) == b(r, c) }
    numAgree * 1.0 / (a.rows * a.cols)

  /**
   * Find the similarity to `needle` of every `needle`-shaped subarray of `haystack`.
   *
   * @param needle Small(er) array to scan across `haystack`.
   * @param haystack Large(r) array in which to look for `needle`s.
   * @tparam T Type of array elements
   * @return List of tuples `(row, col, sim)` representing the similarity between `needle` and the sub-array of
   *         `haystack` of the same size with top-left corner at `(row, col)`.
   */
  def tileSimilarities[T](needle: Array2D[T], haystack: Array2D[T]): List[(Int, Int, Double)] =
    if needle.rows > haystack.rows || needle.cols > haystack.cols then List.empty
    else
      val rDelta = haystack.rows - needle.rows
      val cDelta = haystack.cols - needle.cols

      val indices = for { r <- 0 to rDelta; c <- 0 to cDelta } yield (r, c)

      indices.toList.map { case (r, c) =>
        val slice = haystack.slice(r, r + needle.rows, c, c + needle.cols)
        (r, c, similarity(needle, slice))
      }

  /** Create a new array by stacking `l` and `r` side-by-side. */
  def concat[T: ClassTag](l: Array2D[T], r: Array2D[T]): Array2DBase[T] =
    if l.rows != r.rows 
      then throw new IllegalArgumentException(
      s"Cannot concat two 2d arrays with different number of rows: ${l.rows} and ${r.rows}"
    )
    val data = (0 until l.rows).flatMap( i => l.row(i) ++ r.row(i) ).toArray

    Array2DBase(l.rows, l.cols + r.cols, data)