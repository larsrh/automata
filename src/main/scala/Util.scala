package edu.tum.cs.afl

import scalaz._
import Scalaz._

object Util {

	def seqToInt(seq: Seq[Boolean]): Int =
		if (seq.isEmpty)
			-1
		else
			(0 /: seq) { case (acc, elem) => (elem ? 1 | 0) + 2 * acc }

	def wordToIntWord(word: Seq[Seq[Boolean]]) = (word map seqToInt).mkString("(", ",", ")") 

}
