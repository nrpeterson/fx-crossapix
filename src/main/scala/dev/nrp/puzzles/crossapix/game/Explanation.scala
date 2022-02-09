package dev.nrp.puzzles.crossapix.game

import cats.{Applicative, Monad}
import cats.data.{Chain, OptionT, ReaderWriterState, State}
import cats.mtl.{Stateful, Tell}
import cats.syntax.all.*
import dev.nrp.puzzles.crossapix.game.descriptionparser.DescriptionParser
import monocle.{Focus, Lens}
import monocle.syntax.all.*

sealed trait Description

case class Text(s: String) extends Description
case class LinesReference(lineIds: List[LineId], caption: String) extends Description
case class RegionsReference(regionIds: List[RegionId], caption: String) extends Description

extension (sc: StringContext)
  def desc(args: Any*): List[Description] = DescriptionParser(
    sc.parts.toList.map(StringContext.processEscapes),
    args:_*
  )

case class Explanation(
  stratName: String,
  description: List[Description]
)
val (c1, c2) = (1, 2)
val (l1, l2) = (LineId.Row(5), LineId.Col(6))