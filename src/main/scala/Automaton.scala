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

	sealed trait State {
		protected[afl] def id: Int
		def length: Int
		def words: Set[Seq[Boolean]]

	}

	object EmptySet extends State {
		override def toString = "<empty>"

		protected[afl] val id = 0
		val length = 0
		val words = Set.empty[Seq[Boolean]]

		def ofLength(length: Int): State = ((this: State) /: (1 to length)) { case (s, _) => Succ(s, s) }
	}
	
	object Epsilon extends State {
		override def toString = "<epsilon>"

		protected[afl] val id = 1
		val length = 0
		val words = Set(Seq.empty[Boolean])
	}
	
	final class Succ private(protected[afl] val id: Int, val succ0: State, val succ1: State) extends State {
		require(succ0.length == succ1.length)
		
		override def toString = id + " [0 -> " + succ0.id + ", 1 -> " + succ1.id + "]"

		val length = succ0.length + 1

		def words = {
			val w0 = succ0.words
			val w1 = succ1.words
			(w0 map { false +: _ }) union (w1 map { true +: _ })
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

		def unapply(s: Succ): Option[(State, State)] = Some((s.succ0, s.succ1))
	}

	private var counter = 2

	private val states = mutable.Map[(State, State), State]()

}

class MultiDFA(start: MasterAutomaton.State) extends Automaton {

	val length = start.length
	val dimension = 1

	def words = start.words map { Seq(_) }

}
