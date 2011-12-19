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

		final def padTo(newLength: Int): State =
			(this /: (length until newLength)) { case (s, i) =>
				Succ(s, EmptySet ofLength i)
			}

		private def binaryOp(that: State, onEmptySet: State => State, onEpsilon: => State): State = {
			require(this.length == that.length)
			val cache = mutable.Map[(State, State), State]()

			def aux(s1: State, s2: State): State = {
				assert(this.length == that.length)
				cache.getOrElse((s1, s2), {
					cache.getOrElse((s2, s1), (s1, s2) match {
						case (EmptySet, x) => onEmptySet(x)
						case (x, EmptySet) => onEmptySet(x)
						case (Epsilon, Epsilon) => onEpsilon
						case (Succ(s11, s12), Succ(s21, s22)) =>
							val s = Succ(aux(s11, s21), aux(s12, s22))
							cache((s1, s2)) = s
							s
					})
				})
			}

			aux(this, that)
		}

		@inline final def intersect(that: State): State = binaryOp(that, _ => EmptySet, Epsilon)

		@inline final def union(that: State): State = binaryOp(that, identity, Epsilon)

		final def complement: State = {
			val cache = mutable.Map[State, State]()

			def aux(s: State): State = 
				cache.getOrElse(s, s match {
					case EmptySet => Epsilon
					case Epsilon => EmptySet
					case Succ(s1, s2) =>
						val st = Succ(aux(s1), aux(s2))
						cache(s) = st
						st
				})

			aux(this)
		}
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
				Util.log("Generating new state: " + s)
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
