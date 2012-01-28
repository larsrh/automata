package edu.tum.cs.afl

import collection.mutable

import scalaz._
import Scalaz._

import Util._

/**
 * Utility object to buffer master automata (instances of `MasterAutomaton`).
 */
object MasterAutomaton {

	/**
	 * Common type of all `State`s in all `MasterAutomaton` instances.
	 * Internally, they are called "states", but this doesn't have to
	 * be exposed to interface users. For them, a "state" behaves like
	 * a regular automaton.
	 */
	type Automaton = MasterAutomaton#State 

	private val buffer = mutable.Map[Int, MasterAutomaton]()

	/**
	 * Obtains a `MasterAutomaton` from the buffer of desired dimension
	 * or creates a new one and updates the buffer.
	 */
	def apply(dim: Int) = buffer.getOrElseUpdate(dim, new MasterAutomaton(dim))

}

/**
 * The master automaton, as described in the lecture notes. Lazily contains
 * all possible states for all possible fixed-length languages with the
 * specified dimension.
 * @param dimension must be non-negative
 */
final class MasterAutomaton private(val dimension: Int) { self =>

	require(dimension >= 0)

	/**
	 * Abbreviation for `MasterAutomaton#State` 
	 *
	 * This is the same as `MasterAutomaton.State`.
	 */
	type St = MasterAutomaton#State

	/**
	 * Common behaviour for any state in this automaton.
	 */
	sealed trait State {

		/** Explicit reference to parent master automaton. */
		final def automaton: self.type = self

		protected[afl] def id: Int

		/** The length of the words which are accepted by this state. */
		def length: Int

		/**
		 * The set of all words accepted by this automaton.
		 *
		 * The value of this function does not depend on state but is neither
		 * precomputed nor cached. Avoid intermediate calls.
		 * @return A list of lists of length `self.dimension` containing lists
		 *         of length `this.length`. The result is in natural ordering.
		 */
		def words: List[List[List[Boolean]]]

		/**
		 * Prepends a number of zeroes to this state, producing a new state.
		 * @param newLength the desired length of the new state which must be greater than
		 *        or equal to the current length
		 * @return a new state `s` with `s.length == newLength` if `newLength` is greater
		 *         than or equal to the current length, this state otherwise
		 */
		final def padTo(newLength: Int): State =
			(this /: (length until newLength)) { case (s, i) =>
				val empty = EmptySet ofLength i
				Succ(s :: List.fill((1 << dimension) - 1)(empty))
			}

		/**
		 * Determines the maximum length of `this` and `that` and pads the
		 * state with the smaller length.
		 * @return a pair `(s, t)` such that `s.length == t.length`
		 */
		final def padMax[S <: St](that: S): (State, S) =
			if (this.length < that.length)
				(padTo(that.length), that)
			else if (this.length > that.length)
				(this, that.padTo(length).asInstanceOf[S]) // TODO get rid of cast
			else
				(this, that)

		/**
		 * Common abstraction for union and intersection. The behavior if
		 * either `this` or `that` is empty or both are epsilon can be 
		 * specified. The recursion is the same in both cases.
		 * @param that a state of the same master automaton (which is not
		 *        expressed in the type -- may fail at runtime)
		 */
		private def binaryOp(that: St, onEmptySet: State => State, onEpsilon: => State): State = {
			require(this.automaton eq that.automaton)

			val (_this, _that) = padMax(that.asInstanceOf[State])
			val buffer = mutable.Map[(State, State), State]()

			def aux(s1: State, s2: State): State = {
				assert(s1.length == s2.length)

				buffer.getOrElseUpdate((s1, s2),
					buffer.getOrElse((s2, s1), ((s1, s2): @unchecked) match {
						case (EmptySet, x) => onEmptySet(x)
						case (x, EmptySet) => onEmptySet(x)
						case (Epsilon, Epsilon) => onEpsilon
						case (s1: Succ, s2: Succ) => Succ(s1.succs zip s2.succs map { t => aux(t._1, t._2) })
					})
				)
			}

			aux(_this, _that)
		}

		/** State intersection. */
		@inline final def intersect(that: St): State = binaryOp(that, _ => EmptySet, Epsilon)

		/** State union. */
		@inline final def union(that: St): State = binaryOp(that, identity, Epsilon)

		/** State complement. */
		final def complement: State = {
			val buffer = mutable.Map[State, State]()

			def aux(s: State): State = 
				buffer.getOrElseUpdate(s, s match {
					case EmptySet => Epsilon
					case Epsilon => EmptySet
					case s: Succ => Succ(s.succs map aux)
				})

			aux(this)
		}

		/**
		 * State product.
		 * @return a state `s` with the sum of the dimensions of `this` and `that`
		 */
		final def product(that: St): St = {
			val (_this, _that) = padMax(that)

			val master = MasterAutomaton(that.automaton.dimension + dimension)
			val thatMaster = that.automaton
			
			val buffer = mutable.Map[(State, St), master.State]()

			def aux(s1: State, s2: St): master.State = {
				assert(s1.length == s2.length)

				buffer.getOrElseUpdate((s1, s2),
					// no match here because of a scalac bug
					// (invalid code is generated)
					// appears to be <https://issues.scala-lang.org/browse/SI-4440>
					if (s1 == EmptySet || s2 == thatMaster.EmptySet)
						master.EmptySet
					else if (s1 == Epsilon && s2 == thatMaster.Epsilon)
						master.Epsilon
					else
						master.Succ(
							for (succ1 <- s1.asInstanceOf[Succ].succs; succ2 <- s2.asInstanceOf[MasterAutomaton#Succ].succs)
							yield aux(succ1, succ2)
						)
				)
			}

			aux(_this, _that)
		}

		/**
		 * State join.
		 * The sum of the dimensions of `this` and `that` must be greater than 0.
		 * @param that a state with positive dimension
		 * @return a state `s` with the sum of the dimension of `this` and `that`
		 *         decreased by 2.
		 */
		final def join(that: St): St = {
			require(dimension > 0)
			require(that.automaton.dimension > 0)
			require(dimension + that.automaton.dimension > 2)

			val (_this, _that) = padMax(that)
			val master = MasterAutomaton(that.automaton.dimension + dimension - 2)
			val thatMaster = that.automaton
			
			val buffer = mutable.Map[(State, St), master.State]()

			def aux(s1: State, s2: St): master.State = {
				assert(s1.length == s2.length)
				
				buffer.getOrElseUpdate((s1, s2),
					if (s1 == EmptySet || s2 == thatMaster.EmptySet)
						master.EmptySet
					else if (s1 == Epsilon && s2 == thatMaster.Epsilon)
						master.Epsilon
					else {
						// We use a similar pairing strategy as in `projection` for
						// the case i = 1.
						// Observe that the first half of successors of `s2` share the
						// `false` as the first element (`true` for the second half).
						// For `s1`, every even successor has `false` as the last element.
						val succs1 = s1.asInstanceOf[Succ].succs
						val succs2 = s2.asInstanceOf[MasterAutomaton#Succ].succs

						val (first, second) = succs2 splitAt (succs2.length / 2)

						// the compiler warning about a non-exhaustive match can be safely
						// ignored here
						val pairs = succs1 grouped 2 flatMap { case List(succ1False, succ1True) =>
							(first map { aux(succ1False, _) }) zip
							(second map { aux(succ1True, _) })
						}

						master.Succ(pairs map { case (s1, s2) => s1 union s2 } toList)
					}
				)
			}

			aux(_this, _that)
		}

		/**
		 * Inserts an additional character before the specified position.
		 * @param pos a number from 0 to the dimension of `this` (inclusive)
		 * @return a state with the dimension of `this` increased by 1 and
		 *         the same length as `this`
		 */
		final def insert(pos: Int): St = {
			require(0 <= pos && pos <= dimension)

			val master = MasterAutomaton(dimension + 1)
			val buffer = mutable.Map[State, master.State]()

			def aux(s: State): master.State = {
				def explode(pos: Int, succs: List[State]): List[State] =
					if (pos == 0)
						succs ++ succs
					else {
						val half = succs.length / 2
						val (first, second) = succs splitAt half
						explode(pos - 1, first) ++ explode(pos - 1, second)
					}

				buffer.getOrElseUpdate(s, {
					if (s == EmptySet)
						master.EmptySet
					else if (s == Epsilon)
						master.Epsilon
					else
						master.Succ(explode(pos, s.asInstanceOf[Succ].succs) map aux)
				})
			}

			aux(this)
		}

		/**
		 * State section.
		 * Side effects: logs a warning if `that` has a dimension not equal to 1
		 * @param that a state which should have a dimension of 1
		 * @param pos a number from 1 to the dimension of `this` (inclusive)
		 * @return a state `s` with the dimension of `this` decreased by 1,
		 *         which is empty if `that` has a dimension not equal to 1
		 */
		final def section(that: St, pos: Int): St = {
			require(0 < pos && pos <= dimension)
			require(dimension > 0)

			val (_this, _that) = padMax(that)
			val master = MasterAutomaton(dimension - 1)
			val thatMaster = that.automaton
			
			val buffer = mutable.Map[(State, St), master.State]()

			def aux(s1: State, s2: St): master.State = {
				assert(s1.length == s2.length)
				
				// The pairing strategy is almost the same as in `projection`.
				def groupedUnion(pos: Int, succs: List[State], succFalse: St, succTrue: St): List[master.State] = {
					val half = succs.length / 2
					val (first, second) = succs splitAt half
					if (pos == 1)
						// mutual recursion, yo
						(first map { aux(_, succFalse) }) zip (second map { aux(_, succTrue) }) map { case (s1, s2) => s1 union s2 }
					else
						groupedUnion(pos-1, first, succFalse, succTrue) ++ groupedUnion(pos-1, second, succFalse, succTrue)
				}

				buffer.getOrElseUpdate((s1, s2),
					if (s1 == EmptySet || s2 == thatMaster.EmptySet)
						master.EmptySet
					else if (s1 == Epsilon && s2 == thatMaster.Epsilon)
						master.Epsilon
					else {
						val succs = s1.asInstanceOf[Succ].succs
						val List(succFalse, succTrue) = s2.asInstanceOf[MasterAutomaton#Succ].succs
						master.Succ(groupedUnion(pos, succs, succFalse, succTrue))
					}
				)
			}

			if (that.automaton.dimension != 1) {
				log("Warning: join where m(v) ≠ 1")
				master.EmptySet ofLength length
			}
			else {
				aux(_this, _that)
			}
		}

		/**
		 * State projection. The dimension of `this` must be greater than 1.
		 * @param pos a number from 1 to the dimension of `this` (inclusive)
		 * @return a state `s` with the dimension of `this` decreased by 1
		 */
		final def projection(pos: Int): St = {
			require(0 < pos && pos <= dimension)
			require(dimension > 1)

			val master = MasterAutomaton(dimension - 1)
			val buffer = mutable.Map[State, master.State]()

			// When we project, the set of successors collapes into pairs of successors
			// which can be reached via the same character. Instead of simply calling
			// `groupBy` to find out which, we use a nice symmetry here.
			// Assume we have a dimension of 3. Our initial alphabet is [0,0,0], [0,0,1], ...
			// Observe that if we want to project for i = 1, the first and the fifth, the
			// second and the sixth, ... characters form pairs, respectively.
			// If we project for i = 2, we split the alphabet in the middle and project
			// for i = 1 on both sides.
			def groupedUnion(pos: Int, succs: List[master.State]): List[master.State] = {
				val half = succs.length / 2
				val (first, second) = succs splitAt half
				if (pos == 1)
					(0 until half) map { i => first(i) union second(i) } toList
				else
					groupedUnion(pos-1, first) ++ groupedUnion(pos-1, second)
			}

			def aux(s: State): master.State =
				buffer.getOrElseUpdate(s, s match {
					case EmptySet => master.EmptySet
					case Epsilon => master.Epsilon
					case s: Succ => master.Succ(groupedUnion(pos, s.succs map aux))
				})

			aux(this)
		}

	}

	/**
	 * The state representing the empty set.
	 *
	 * This state is only valid for words of length 0. For greater lengths,
	 * this state has to be padded (preferably via `ofLength`).
	 */
	object EmptySet extends State {
		override def toString = "<empty>"

		protected[afl] val id = 0
		val length = 0
		val words = List.empty[List[List[Boolean]]]

		/**
		 * Produces a state which accepts no words of the desired length.
		 * @param length must be a non-negative number
		 * @return a state `s` such that `s.length == length` and `s.words` empty
		 */
		def ofLength(length: Int): State = ((this: State) /: (1 to length)) { case (s, _) => Succ(List.fill(1 << dimension)(s)) }
	}

	/**
	 * The "final" state which accepts epsilon.
	 */
	object Epsilon extends State {
		override def toString = "<epsilon>"

		protected[afl] val id = 1
		val length = 0
		val words = List(List.fill(dimension)(List.empty[Boolean]))
	}
	
	/**
	 * A non-final state which reads an input character and advances to the
	 * corresponding successor state.
	 *
	 * The resulting `length` of this state is the common length of the
	 * successor states increased by 1.
	 *
	 * This state doesn't store the characters of the alphabet, as the
	 * successors must have a fixed ordering. For a dimension of 2, this
	 * would be: [0,0], [0,1], [1,0], [1,1].
	 *
	 * @param succs A list containing 2 to the power of the dimension of `this`
	 *        successor states. All successors must have the same `length`. The
	 *        list must be in natural order with respect to the corresponding
	 *        character.
	 */
	final class Succ private(protected[afl] val id: Int, val succs: List[State]) extends State {
		require(succs.length == 1 << dimension)
		require(succs.groupBy(_.length).size == 1)
		
		override def toString = id + (succs zip chars(dimension) map { case (succ, c) =>
			(c map { _ ? '1' | '0' } mkString "") + "->" + succ.id
		}).mkString(" [", ", ", "]")

		val length = succs.head.length + 1

		def words =
			succs zip chars(dimension) >>= { case (s, c) =>
				s.words map { w =>
					c zip w map { case (h, t) => h :: t }
				}
			}
	}

	/**
	 * Utility object which queries the buffer of states and updates it
	 * if necessary.
	 */
	object Succ {

		/**
		 * Obtains a `State` from the buffer with the desired successors
		 * or creates a new `State` and updates the buffer.
		 */
		def apply(succs: List[State]): State = states.getOrElseUpdate(succs, {
			val s = new Succ(counter, succs)
			Util.log("Generating new state: " + s)
			counter += 1
			s
		})
	}

	private var counter = 2

	private val states = mutable.Map[List[State], State]()

}

