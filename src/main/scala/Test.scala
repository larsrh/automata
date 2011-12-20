package edu.tum.cs.afl

import scalaz._
import Scalaz._

object Test extends App {

	import Util._

	object One {

		val m1 = MasterAutomaton(1)
		import m1._

		def assertContains(s: MasterAutomaton#State, words: Int*) =
			assert((s.words map { ws => seqToInt(ws.head) }).toSet === words.toSet)

		def succ(s1: State, s2: State) = Succ(List(s1, s2))

		val s1 = succ(EmptySet, Epsilon)
		assertContains(s1, 1)

		val s2 = succ(Epsilon, EmptySet)
		assertContains(s2, 0)

		val s12 = succ(s1, s2)
		assertContains(s12, 1, 2)

		val s1pad = s1 padTo 2
		assertContains(s1pad, 1)

		val inter = s12 intersect s1pad
		assertContains(inter, 1)

		val s2pad = s2 padTo 2
		assertContains(s2pad, 0)

		val union = s12 union s2pad
		assertContains(union, 0, 1, 2)

		val compl = union.complement
		assertContains(compl, 3)

		val unionPad = union padTo 3
		assertContains(unionPad, 0, 1, 2)

		val complPad = unionPad.complement
		assertContains(complPad, 3, 4, 5, 6, 7)

		val empty = unionPad intersect complPad
		assertContains(empty)

	}

	object Two {

		val m2 = MasterAutomaton(2)
		import m2._

		def assertContains(s: MasterAutomaton#State, words: (Int, Int)*) =
			assert((s.words map { _ map seqToInt } toSet) === (words map { case (w1, w2) => List(w1, w2) } toSet))

		def succ(s00: State, s01: State, s10: State, s11: State) = Succ(List(s00, s01, s10, s11))

		val s3 = succ(Epsilon, EmptySet, EmptySet, EmptySet)
		assertContains(s3, (0, 0))

		val s2 = succ(EmptySet, EmptySet, EmptySet, Epsilon)
		assertContains(s2, (1, 1))

		val s4 = succ(EmptySet padTo 1, EmptySet padTo 1, s2, s3)
		assertContains(s4, (3, 1), (2, 2))

		val union = s2 padTo 2 union s4
		assertContains(union, (3, 1), (2, 2), (1, 1))

		val compl = s2.complement
		assertContains(compl, (0, 0), (0, 1), (1, 0))

	}

	object Three {
		
		def assertContains(s: MasterAutomaton#State, words: (Int, Int, Int)*) =
			assert((s.words map { _ map seqToInt } toSet) === (words map { case (w1, w2, w3) => List(w1, w2, w3) } toSet))

		val product = One.union product Two.union
		assertContains(product, (for (x <- List(0, 1, 2); y <- List((3, 1), (2, 2), (1, 1))) yield (x, y._1, y._2)): _*)

		val projection = product projection 2
		Two.assertContains(projection, List(0, 1, 2) <|*|> List(1, 2): _*)

	}

	Three // force initialization

}

