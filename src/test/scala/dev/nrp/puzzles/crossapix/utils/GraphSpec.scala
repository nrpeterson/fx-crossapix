package dev.nrp.puzzles.crossapix.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*


class GraphSpec extends AnyFlatSpec with Matchers:
  val graph: Graph[Int] = Graph(
    List(1, 2, 3, 4, 5, 6),
    Map(
      1 -> List(2, 3),
      2 -> List(1, 3),
      3 -> List(1, 2),
      4 -> List(5),
      5 -> List(4),
      6 -> List.empty
    )
  )

  val step = Graph.stepDFS(graph)

  val s1 = DFSState[Int](List.empty, Map.empty, Map.empty)
  val s2 = DFSState[Int](
    List.empty,
    Map(1->None, 2->Some(1), 3->Some(2)),
    Map(1->List(2), 2->List(3), 3->List.empty)
  )
  val s3 = DFSState[Int](
    List.empty,
    Map(1->None, 2->Some(1), 3->Some(2), 4->None, 5->Some(4), 6->None),
    Map(1->List(2), 2->List(3), 3->List.empty, 4->List(5), 5->List.empty, 6->List.empty)
  )
  val s4 = DFSState[Int](List(1->None), Map.empty, Map.empty)
  val s5 = DFSState[Int](
    List(4->None, 5->None, 6->None),
    Map(1->None, 2->Some(1), 3->Some(2)),
    Map(1->List(2), 2->List(3), 3->List.empty)
  )
  val s6 = DFSState[Int](
    List(3->Some(2), 3->Some(1)),
    Map(1->None, 2->Some(1)),
    Map(1->List(2))
  )
  val s7 = DFSState[Int](
    List(3->Some(1)),
    Map(1->None, 2->Some(1), 3->Some(2)),
    Map(1->List(2), 2->List(3))
  )

  val allStates = List(s1, s2, s3, s4, s5, s6, s7)
  val emptyStackStates = List(s1, s2, s3)
  val nonEmptyStackStates = List(s4, s5, s6, s7)

  "Graph.fromEdges" should "use the given vertex list" in {
    val verts = (0 until 4).toList
    val edges = List.empty
    Graph.fromEdges(verts, edges).verts shouldEqual verts
  }

  it should "correctly create the adjacency list" in {
    val verts = (0 until 5).toList
    val edges = List((0, 1), (2, 0), (2, 1), (2, 3))

    val result = Graph.fromEdges(verts, edges)

    result.adjList(0) should contain theSameElementsAs List(1, 2)
    result.adjList(1) should contain theSameElementsAs List(0, 2)
    result.adjList(2) should contain theSameElementsAs List(0, 1, 3)
    result.adjList(3) should contain theSameElementsAs List(2)
    result.adjList(4) should contain theSameElementsAs List.empty
  }

  "Graph.stepDFS" should "return None if the stack is empty, and the first vertex otherwise" in {

    allStates.foreach { s =>
      val result = step.value.runA(s).value
      if s.stack.isEmpty then result shouldEqual None
      else result shouldEqual Some(s.stack.head._1)
    }
  }

  it should "add the head of the stack to the parents map if the vertex isn't already mapped" in {
    allStates.foreach { s =>
      val result: DFSState[Int] = step.value.runS(s).value

      if s.stack.isEmpty then result.parents shouldEqual s.parents
      else
        val (v, p) = s.stack.head

        result.parents should contain key v

        if s.parents.contains(v) then result.parents shouldEqual s.parents
        else result.parents(v) shouldEqual p
    }
  }

  it should "update the children map using the head of the stack if the vertex isn't already mapped" in {
    allStates.foreach { s =>
      val result: DFSState[Int] = step.value.runS(s).value

      if s.stack.isEmpty then result.children shouldEqual s.children
      else
        val (v, pOpt) = s.stack.head
        pOpt match
          case None => result.children shouldEqual s.children
          case Some(p) =>
            val origChildren = s.children.getOrElse(p, List.empty)

            if s.parents.contains(v) then result.children shouldEqual s.children
            else
              result.children should contain key p
              origChildren.foreach { c => result.children(p) should contain (c) }
              result.children(p) should contain (v)
    }
  }

  it should "pop the stack and add all unvisited children to the top of the stack with proper parent" in {
    allStates.foreach { s =>
      val result: DFSState[Int] = step.value.runS(s).value

      if s.stack.isEmpty then result shouldEqual s
      else
        val (v, pOpt) = s.stack.head
        val rest = s.stack.tail

        result.stack.takeRight(rest.size) shouldEqual rest

        val newElems = result.stack.dropRight(rest.size)

        newElems.foreach { case (w, q) =>
          q shouldEqual Some(v)
          graph.adjList(v) should contain (w)
        }

        val justVerts = newElems.map(_._1)
        graph.adjList(v).foreach { w =>
          assert(justVerts.contains(w) || result.parents.contains(w))
        }
    }
  }

  "Graph.fullDFS" should "generate the full DFS forest" in {
    val DFSState(stack, parents, children) = Graph.fullDFS(graph)

    stack shouldBe empty

    parents shouldEqual Map(1->None, 2->Some(1), 3->Some(2), 4->None, 5->Some(4), 6->None)
    children shouldEqual Map(1->List(2), 2->List(3), 4->List(5))
  }

  "Graph.connectedComponents" should "find the right components!" in {
    val comps = Graph.connectedComponents(graph)
    comps should have size 3

    val correct = List(List(1, 2, 3), List(4, 5), List(6))
    correct.foreach { comp =>
      assert(comps.exists { _.sorted == comp })
    }
  }

