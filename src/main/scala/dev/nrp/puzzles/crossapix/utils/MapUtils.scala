package dev.nrp.puzzles.crossapix.utils

object MapUtils:
  def zipWith[K, V1, V2, O](f: (V1, V2) => O, m1: Map[K, V1], m2: Map[K, V2]): Map[K, O] =
    m1.map { case (k, v1) => (k, (v1, m2.get(k))) }
      .collect { case (k, (v1, Some(v2))) => (k, f(v1, v2)) }
  
  def zip[K, V1, V2](m1: Map[K, V1], m2: Map[K, V2]): Map[K, (V1, V2)] = zipWith(Tuple2.apply[V1, V2], m1, m2)
    
