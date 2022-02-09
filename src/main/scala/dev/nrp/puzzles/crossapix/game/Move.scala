package dev.nrp.puzzles.crossapix.game

import monocle.syntax.all.*

case class Move(
  regionId: RegionId,
  state: RegionState
)