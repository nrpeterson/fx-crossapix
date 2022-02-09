package dev.nrp.puzzles.crossapix.utils

import cats.data.{OptionT, State}
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import monocle.syntax.all.*

import scala.annotation.tailrec

case class Graph[Vertex](verts: List[Vertex], adjList: Map[Vertex, List[Vertex]])

case class DFSState[Vertex](stack: List[(Vertex, Option[Vertex])], parents: Map[Vertex, Option[Vertex]], children: Map[Vertex, List[Vertex]])

object Graph:
  def fromEdges[Vertex](verts: List[Vertex], edges: List[(Vertex, Vertex)]): Graph[Vertex] =
    val base: Map[Vertex, Set[Vertex]] = verts.map(_ -> Set.empty).toMap
    val adjList =
      edges
        .foldLeft(base) { case (cur, (u, v)) =>
          cur.focus(_.index(u)).modify(_ + v)
            .focus(_.index(v)).modify(_ + u)
        }
        .map { case (k, v) => (k, v.toList) }

    Graph(verts, adjList)
  
  def stepDFS[Vertex](graph: Graph[Vertex]): OptionT[[A] =>> State[DFSState[Vertex], A], Vertex] =
    def get: OptionT[[A] =>> State[DFSState[Vertex], A], DFSState[Vertex]] = OptionT.liftF(State.get[DFSState[Vertex]])
    def modify(f: DFSState[Vertex] => DFSState[Vertex]): OptionT[[A] =>> State[DFSState[Vertex], A], Unit] =
      OptionT.liftF(State.modify(f))

    def visitVertex(vertex: Vertex, parent: Option[Vertex])(dfsState: DFSState[Vertex]): DFSState[Vertex] =
      if dfsState.parents.contains(vertex) then dfsState.focus(_.stack).modify(_.tail)
      else
        val discovered = graph.adjList(vertex).filter(!dfsState.parents.contains(_))

        val updatedChildrenMaybe = parent.map { p =>
          val curList = dfsState.children.getOrElse(p, List.empty)
          val newList = curList.appended(vertex)
          dfsState.focus(_.children).modify(_.updated(p, newList))
        }
        val updatedChildren = updatedChildrenMaybe.getOrElse(dfsState)

        updatedChildren
          .focus(_.stack).modify { stack => discovered.map(_ -> Some(vertex)) ++ stack.tail }
          .focus(_.parents).modify(_ + (vertex -> parent))

    for {
      st <- get
      (v, p) <- OptionT.fromOption(st.stack.headOption)
      _ <- modify(visitVertex(v, p))
    } yield v

  def fullDFSRunner[Vertex](graph: Graph[Vertex]): State[DFSState[Vertex], Unit] =
    val opt: OptionT[[A] =>> State[DFSState[Vertex], A], Unit] = LazyList.continually(stepDFS(graph)).sequence_
    opt.value.map(_ => ())

  def fullDFS[Vertex](g: Graph[Vertex]): DFSState[Vertex] =
    val init = DFSState[Vertex](g.verts.map(_->None), Map.empty, Map.empty)
    fullDFSRunner(g).runS(init).value

  def allDescendants[Vertex](v: Vertex, children: Map[Vertex, List[Vertex]]): List[Vertex] =
    @tailrec
    def helper(stack: List[Vertex], seen: List[Vertex]): List[Vertex] = stack match
      case Nil => seen
      case h :: tail => helper(children.getOrElse(h, List.empty) ++ tail, h :: seen)

    helper(List(v), List.empty)

  def connectedComponents[Vertex](g: Graph[Vertex]): List[List[Vertex]] =
    val DFSState(_, parent, children) = fullDFS(g)
    val roots = parent.collect { case (v, None) => v }
    roots.toList.map { v => allDescendants(v, children) }