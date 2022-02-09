package dev.nrp.puzzles.crossapix.vision

import java.awt.{Color, Graphics}
import java.awt.image.BufferedImage
import java.io.File

import cats.effect.IO
import dev.nrp.puzzles.crossapix.game.*
import dev.nrp.puzzles.crossapix.utils.*
import javax.imageio.ImageIO

object BoardReader:

  /**
   * Given a [[java.io.File]] that points to a screenshot of a board, extract that board.
   *
   * This is currently super hacky and will fail on numerous GOOD cases, much less bad ones.
   *
   * @param file [[java.io.File]] that points to a screen of a board
   * @return [[dev.nrp.puzzles.crossapix.game.Board]] parsed from the screenshot
   */
  def parseBoardFromFile(file: File): IO[Board] = IO(ImageIO.read(file)).map(parseBoardFromImage)

  /**
   * Given a [[java.awt.image.BufferedImage]] which is a screenshot of a board, extract that board.
   *
   * This is currently super hacky and will fail on numerous GOOD cases, much less bad ones.
   *
   * @param img [[java.awt.image.BufferedImage]] containing a screenshot of a board
   * @return [[dev.nrp.puzzles.crossapix.game.Board]] parsed from the screenshot
   */
  def parseBoardFromImage(img: BufferedImage): Board =
    val arr = arrayFromImage(img)
//    println(arr.mkString())
    val Components(rows, cols, rowTotalArrays, colTotalArrays, spaceArrays) = boardAndTotalArrays(arr)
//    println(rowTotalArrays)
    val rowTotals = rowTotalArrays.map(OCRUtils.extractNumber).toVector
    val colTotals = colTotalArrays.map(OCRUtils.extractNumber).toVector
    val regions = regionsFromSpaceArrays(rows, cols, spaceArrays)
    Board(rows, cols, rowTotals, colTotals, regions)

  /**
   * Convert an image to a 2-dimensional binary array by converting to grayscale and thresholding.
   *
   * Note that the threshold is NOT anything intelligent -- it is currently a fixed threshold.
   *
   * @param img [[java.awt.image.BufferedImage]] to convert
   * @return An [[dev.nrp.puzzles.crossapix.utils.Array2D[Int]] with values 0 and 1 representing the image
   */
  def arrayFromImage(img: BufferedImage): Array2DBase[Int] =
    val grayscale = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    for { r <- 0 until img.getHeight; c <- 0 until img.getWidth } do {
      val color = new Color(img.getRGB(c, r))
      val (red, green, blue) = (color.getRed, color.getGreen, color.getBlue)
      val gray = ((red + green + blue) / 3.0).toInt
      grayscale.setRGB(c, r, new Color(gray, gray, gray).getRGB)
    }

    Array2D.fromNestedSeqs(
      (0 until img.getHeight).toList.map { r =>
        (0 until img.getWidth).toList.map { c =>
          val base = new Color(img.getRGB(c, r)).getRed
          if base > 190 then 0 else 1
        }
      }
    )

  /**
   * Container for array views representing key parts of a CrossAPix game.
   *
   * @param rows Number of rows found in the grid
   * @param cols Number of columns found in the grid
   * @param rowTotalArrays Arrays extracted from the images of the individual row totals (one per row)
   * @param colTotalArrays Arrays extracted from the images of the individual column totals (one per column)
   * @param spaceArrays Nested list containing array views of the spaces of the grid (without the boundary), in
   *                    row-major order.
   */
  case class Components(
    rows: Int,
    cols: Int,
    rowTotalArrays: List[Array2D[Int]],
    colTotalArrays: List[Array2D[Int]],
    spaceArrays: List[List[Array2D[Int]]]
  )

  /**
   * Extract array views containing key board features (row/column totals and grid spaces).
   *
   * @param arr [[dev.nrp.puzzles.crossapix.utils.Array2D[Int]], with 0-1 values, representing a CrossAPix game
   * @return a [[Components]] instance with:
   *         - The number of rows and columns found
   *         - Arrays representing images of each row/column total
   *         - One array per space on the board representing the image on the interior of the space
   */
  def boardAndTotalArrays(arr: Array2D[Int]): Components =
    val trimmed = Array2D.trim(arr, 0)
//    println("trimmed")
//    println(trimmed.mkString())
    val numsBottom = trimmed.findFirstRow(_.forall(_ == 0)).get + 1
    val numsRight = trimmed.findFirstCol(_.forall(_ == 0)).get + 1
    val gridTop = (numsBottom until trimmed.rows).find { r => trimmed.row(r).exists(_ != 0) }
      .getOrElse(numsBottom)
    val gridLeft = (numsRight until trimmed.cols).find { c => trimmed.col(c).exists(_ != 0) }
      .getOrElse(numsRight)

    // Note that gridTop and gridLeft correspond to non-zero row/col, and so the trimmed array will still have the same
    // top-left corner.  This is important for ease of parsing the row/column numbers.
    val grid = Array2D.trim(trimmed.slice(gridTop, trimmed.rows, gridLeft, trimmed.cols), 0)

//    println("grid")
//    println(grid.mkString())

    val (gridSpec, gridSpaces) = Array2D.extractGrid(grid, 1)
    val rows = gridSpec.rows
    val cols = gridSpec.cols

    val rowSumGrid = trimmed.slice(gridTop, trimmed.rows, 0, numsRight)
    val rowSumSegments = Array2D.getRowRegions(rowSumGrid, gridSpec)

    val colSumGrid = trimmed.slice(0, numsBottom, gridLeft, trimmed.cols)
    val colSumSegments = Array2D.getColRegions(colSumGrid, gridSpec)

    Components(
      rows,
      cols,
      rowSumSegments,
      colSumSegments,
      gridSpaces
    )

  /**
   * Given arrays representing binarized images of the inside of the grid spaces, extract the structure of the board.
   *
   * Each array represents a binarized image of the inside of a grid space. As only the consistent grid lines are
   * removed, we can therefore tell whether a given side of that space has a thick border by whether or not there is
   * still boundary on that side.
   *
   * @param rows Number of rows
   * @param cols Number of columns
   * @param spaces Binarized images (as 0-1 2-dimensional arrays) of the insides of the grid spaces
   * @return Map from (arbitrary) region IDs to the list of board spaces they contain.
   */
  def regionsFromSpaceArrays(rows: Int, cols: Int, spaces: List[List[Array2D[Int]]]): Map[RegionId, List[Space]] =
    // Build the regions map
    type Pos = (Int, Int)

    def edgeUp(r: Int, c: Int, arr: Array2D[Int]): Option[Pos] =
      if r == 0 || arr.row(0).forall(_ == 1) then None
      else Some((r-1, c))

    def edgeDown(r: Int, c: Int, arr: Array2D[Int]): Option[Pos] =
      if r == rows - 1 || arr.row(arr.rows - 1).forall(_ == 1) then None
      else Some((r+1, c))

    def edgeLeft(r: Int, c: Int, arr: Array2D[Int]): Option[Pos] =
      if c == 0 || arr.col(0).forall(_ == 1) then None
      else Some((r, c-1))

    def edgeRight(r: Int, c: Int, arr: Array2D[Int]): Option[Pos] =
      if c == cols - 1 || arr.col(arr.cols - 1).forall(_ == 1) then None
      else Some((r, c+1))

    def toAdj(r: Int, c: Int, arr: Array2D[Int]): (Pos, List[Pos]) =
      val neighbors = List(edgeUp, edgeDown, edgeLeft, edgeRight)
        .map(_(r, c, arr))
        .collect { case Some(p) => p }
      ((r, c), neighbors)


    val verts = for { r <- (0 until rows).toList; c <- (0 until cols).toList } yield (r, c)
    val adjList: Map[Pos, List[Pos]] = ListUtils.mapWithIndex2(toAdj)(spaces).flatten.toMap
    val graph = Graph(verts, adjList)
    val regions = Graph.connectedComponents(graph)
      .sortBy(_.min)
      .zipWithIndex
      .map { case (c, i) => (i, c.map(Space.apply.tupled)) }
      .toMap

    regions