package dev.nrp.puzzles.crossapix.utils

import monocle.syntax.all.*

object ListUtils:
  def zipWithIndex2[A](list: List[List[A]]): Iterable[((Int, Int), A)] =
    def mapInside(elem: (List[A], Int)): Iterable[((Int, Int), A)] =
      val (vals, i) = elem
      vals.zipWithIndex.map(t => ((i, t._2), t._1))

    list.zipWithIndex.flatMap(mapInside)

  def mapWithIndex2[A, B](f: (Int, Int, A) => B)(list: List[List[A]]): List[List[B]] =
    list
      .zipWithIndex
      .map { case (l, i) => l.zipWithIndex.map { case (a, j) => f(i, j, a) } }

  def toCounts[A](list: List[A]): Map[A, Int] = list.groupMapReduce(identity)(_ => 1)(_ + _)
  
  def hierarchyCounts[A, B](list: List[(A, B)]): Map[A, Map[B, Int]] =
    list.groupMapReduce { tup => tup } { _ => 1 } { _ + _ }
      .groupMapReduce { case ((a, _), _) => a } { case ((_, b), c) => Map(b->c) } { _ ++ _ }

  def compress[A](list: Seq[A]): List[(A, Int)] =
    val backward = list.foldLeft(List.empty[(A, Int)]) { (cur, value) =>
      cur match
        case (a, i) :: rest if a == value => (a, i+1) :: rest
        case other => (value, 1) :: other
    }
    backward.reverse

