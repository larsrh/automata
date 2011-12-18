package edu.tum.cs.afl

import collection.mutable

trait Automaton {

	def dimension: Int
	def length: Int

	/**
	 * The set of all words accepted by this automaton.
	 * @return a `Set` of sequences of length `this.dimension` containing
	 *         sequences of length `this.length`
	 */
	def words: Set[Seq[Seq[Boolean]]]

}

object MasterAutomaton {

	sealed trait State extends Automaton {
		def id: Int
		def length: Int

		override final def hashCode = id
		
		final val dimension = 1
	}

	object EmptySet extends State {
		override def toString = "<empty>"

		val id = 0
		val length = 0
		val words = Set.empty[Seq[Seq[Boolean]]]
	}
	
	object Epsilon extends State {
		override def toString = "<epsilon>"

		val id = 1
		val length = 0
		val words = Set(Seq(Seq.empty[Boolean]))
	}
	
	class Succ private(val id: Int, val succ0: State, val succ1: State) extends State {
		require(succ0.length == succ1.length)
		
		override def toString = id + " [0 -> " + succ0.id + ", 1 -> " + succ1.id + "]"

		val length = succ0.length + 1

		def words = {
			def prepend(prefix: Boolean)(xss: Seq[Seq[Boolean]]) =
				xss map { xs => prefix +: xs }

			val w0 = succ0.words
			val w1 = succ1.words
			(w0 map prepend(false)) union (w1 map prepend(true))
		}
	}

	object Succ {
		def apply(succ0: State, succ1: State): State = {
			val succs = (succ0, succ1)
			states.getOrElse(succs, {
				val s = new Succ(counter, succ0, succ1)
				counter += 1
				states(succs) = s
				s
			})
		}
	}

	private var counter = 2

	private val states = mutable.Map[(State, State), State]()

}

class MultiDFA(start: MasterAutomaton.State) extends Automaton {

	val length = start.length
	val dimension = start.dimension

	def words = start.words

}
