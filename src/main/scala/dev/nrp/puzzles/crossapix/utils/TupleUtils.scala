package dev.nrp.puzzles.crossapix.utils

import scala.util.NotGiven

type Insert[A, T <: Tuple] <: Tuple = T match
  case EmptyTuple => A *: EmptyTuple
  case A *: rest => A *: rest
  case h *: rest => h *: Insert[A, rest]

type Union[S <: Tuple, T <: Tuple] <: Tuple = T match
  case EmptyTuple => S
  case h *: t => Union[Insert[h, S], t]

trait Find[Needle, Haystack <: Tuple]:
  def apply(h: Haystack): Needle

object Find:
  def apply[Needle, Haystack <: Tuple](using ev: Find[Needle, Haystack]): Find[Needle, Haystack] = ev

  given isMatch[N, T <: Tuple]: Find[N, N *: T] with
    def apply(h: N *: T): N = h.head

  given isNotMatch[N, H, T <: Tuple](using NotGiven[N =:= H], Find[N, T]): Find[N, H *: T] with
    def apply(h: H *: T): N = Find[N, T](h.tail)

trait Select[Needles <: Tuple, Haystack <: Tuple]:
  def apply(h: Haystack): Needles

object Select:
  def apply[Needles <: Tuple, Haystack <: Tuple](using ev: Select[Needles, Haystack]): Select[Needles, Haystack] = ev

  given[H <: Tuple]: Select[EmptyTuple, H] with
    def apply(h: H): EmptyTuple = EmptyTuple

  given[N, Ns <: Tuple, H <: Tuple](using Find[N, H], Select[Ns, H]): Select[N *: Ns, H] with
    def apply(h: H): N *: Ns = Find[N, H](h) *: Select[Ns, H](h)
