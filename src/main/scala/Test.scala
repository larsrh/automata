package edu.tum.cs.afl

import scalaz._
import Scalaz._

object Test extends App {

	import MasterAutomaton._
	import Util._

	def assertContains(s: State, words: Int*) =
		assert((s.words map seqToInt) === words.toSet)

	val s1 = Succ(EmptySet, Epsilon)
	assertContains(s1, 1)

	val s2 = Succ(Epsilon, EmptySet)
	assertContains(s2, 0)

	val s12 = Succ(s1, s2)
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

}

