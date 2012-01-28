package edu.tum.cs.afl

import io.Source
import annotation.elidable
import elidable._
import collection.mutable

import java.io.{PrintWriter, FileOutputStream}

import scalaz._
import Scalaz._

/** Utility object. */
object Util {

	/** Converts a sequence of `Boolean`s in MSBF order to an integer. */
	def seqToInt(seq: Seq[Boolean]): Int =
		if (seq.isEmpty)
			-1
		else
			(0 /: seq) { case (acc, elem) => (elem ? 1 | 0) + 2 * acc }

	/**
	 * Converts a sequence of a sequence in MSBF order to a string representation
	 * in decadic encoding.
	 */
	def wordToIntWord(word: Seq[Seq[Boolean]]) = (word map seqToInt).mkString(" ") 

	private val charsBuffer = mutable.Map(0 -> List(List.empty[Boolean]))

	def chars(dimension: Int): List[List[Boolean]] = charsBuffer.getOrElse(dimension, {
		val shorter = chars(dimension-1)
		val res = (shorter map { false :: _ }) ++ (shorter map { true :: _ })
		charsBuffer(dimension) = res
		res
	})

	@elidable(FINE)
	def log(x: Any) = Console.err println x

	type =>?[-A, +B] = PartialFunction[A, B]

	def readFile(file: String) = Source fromFile file mkString
	
	def writeFile(file: String, lines: Seq[String]) = {
		val stream = new PrintWriter(new FileOutputStream(file))
		lines foreach { stream println }
		stream.flush()
		stream.close()
	}

}
