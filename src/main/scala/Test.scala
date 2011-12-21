package edu.tum.cs.afl

import scalaz._
import Scalaz._

import MasterAutomaton._
import Util._

object Test extends App {

	object One {

		val m1 = MasterAutomaton(1)
		import m1._

		def assertContains(s: Automaton, words: Int*) =
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

		def assertContains(s: Automaton, words: (Int, Int)*) =
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
		
		def assertContains(s: Automaton, words: (Int, Int, Int)*) =
			assert((s.words map { _ map seqToInt } toSet) === (words map { case (w1, w2, w3) => List(w1, w2, w3) } toSet))

		val product = One.union product Two.union
		assertContains(product, (for (x <- List(0, 1, 2); y <- List((3, 1), (2, 2), (1, 1))) yield (x, y._1, y._2)): _*)

		val projection = product projection 2
		Two.assertContains(projection, List(0, 1, 2) <|*|> List(1, 2): _*)

		val join = product join Two.union
		assertContains(join, (for (x <- List(0, 1, 2); y <- List((3, 1), (2, 2), (1, 1))) yield (x, y._1, y._2)): _*)

		val join2 = One.s1pad join product
		Two.assertContains(join2, (3, 1), (2, 2), (1, 1))

		val section = product.section(One.compl, 2)
		Two.assertContains(section, (0, 1), (1, 1), (2, 1))

	}

	object Parsers {

		import Parser._
		import Two._

		val f1 =
"""2
2
digraph G {
4[shape=diamond];
1[peripheries=2];
4 -> 2 [label="10 "];
4 -> 3 [label="11 "];
2 -> 1 [label="11 "];
3 -> 1 [label="00 "];
}"""

		val af1 = parseAutomaton(f1)
		val a = af1.fold(sys.error, identity)
		Two.assertContains(a, (3, 1), (2, 2))

		val f2 =
"""1
1
digraph G {
2[shape=diamond];
1[peripheries=2];
2 -> 1 [label="0 1 "];
}
"""

		val af2 = parseAutomaton(f2)
		val b = af2.fold(sys.error, identity)
		One.assertContains(b, 0, 1)

		val c = a.section(b, 2)
		One.assertContains(c, 3)

		val d = c union b
		One.assertContains(d, 0, 1, 3)

		val e = d product c
		Two.assertContains(e, (0, 3), (1, 3), (3, 3))
		
	}

	// force initialization
	Three
	Parsers

}

