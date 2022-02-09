package dev.nrp.puzzles.crossapix.heuristics

import cats.{Functor, Monad}
import cats.data.{Chain, OptionT, WriterT}
import cats.mtl.{Ask, Stateful, Tell}
import cats.syntax.all.*
import dev.nrp.puzzles.crossapix.attributes.Attributes
import dev.nrp.puzzles.crossapix.game.{Board, SolutionStep}
import dev.nrp.puzzles.crossapix.utils.*

trait Heuristic[H]:
  type Attrs <: Tuple
  def nextStep(attrs: Attrs): Option[SolutionStep]

object Heuristic:
  def apply[H](using ev: Heuristic[H]): Heuristic[H] = ev

  type Aux[H, As <: Tuple] = Heuristic[H] { type Attrs = As }

  given base: Aux[EmptyTuple, EmptyTuple] = new Heuristic[EmptyTuple]:
    type Attrs = EmptyTuple
    def nextStep(attrs: EmptyTuple): Option[SolutionStep] = None

  given recurse[H, SH <: Tuple, T <: Tuple, ST <: Tuple](using
    hHeur: Aux[H, SH],
    tHeur: Aux[T, ST],
    hSel: Select[SH, Union[SH, ST]],
    tSel: Select[ST, Union[SH, ST]]
  ): Aux[H *: T, Union[SH, ST]] = new Heuristic[H *: T]:
    type Attrs = Union[SH, ST]
    def nextStep(attrs: Attrs): Option[SolutionStep] = 
      hHeur.nextStep(hSel(attrs))
        .orElse(tHeur.nextStep(tSel(attrs)))

trait Heuristic1[H] extends Heuristic[H]:
  type Attr
  type Attrs = Tuple1[Attr]
  
  def getStep(attr: Attr): Option[SolutionStep]
  def nextStep(attrs: Attrs): Option[SolutionStep] = getStep(attrs.head)
  
trait Heuristic2[H] extends Heuristic[H]:
  type Attr1
  type Attr2
  type Attrs = (Attr1, Attr2)
  
  def getStep(attr1: Attr1, attr2: Attr2): Option[SolutionStep]
  def nextStep(attrs: Attrs): Option[SolutionStep] = getStep(attrs(0), attrs(1))

trait Heuristic3[H] extends Heuristic[H]:
  type Attr1
  type Attr2
  type Attr3
  type Attrs = (Attr1, Attr2, Attr3)

  def getStep(attr1: Attr1, attr2: Attr2, attr3: Attr3): Option[SolutionStep]
  def nextStep(attrs: Attrs): Option[SolutionStep] = getStep(attrs(0), attrs(1), attrs(2))