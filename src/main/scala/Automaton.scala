package edu.tum.cs.afl

import collection.{mutable, breakOut}

import scalaz._
import Scalaz._

import Util._

object MasterAutomaton {

	private val buffer = mutable.Map[Int, MasterAutomaton]()

	def apply(dim: Int) = buffer.getOrElseUpdate(dim, new MasterAutomaton(dim))

	def fromEdges(start: Int, edges: Seq[(Int, Int, Seq[Seq[Boolean]])], end: Int, length: Int, dimension: Int): MasterAutomaton#State = {
		require(dimension > 0)
		require(length >= 0)

		val transitions = mutable.Map[(Int, Seq[Boolean]), mutable.Set[Int]]()
		val master = MasterAutomaton(dimension)
		val buffer = mutable.Map[Set[Int], master.State]()

		def aux(len: Int, states: Set[Int]): master.State = buffer.getOrElse(states, {
			// we don't use `getOrElseUpdate` here because buffering `(len, states)` pairs
			// has too much overhead as every `states` set has an unique `len` (except the
			// empty set, which may occur as the trap state)
			if (states.isEmpty)
				master.EmptySet ofLength len
			else if (states contains end)
				master.Epsilon
			else {
				val res = master.Succ(chars(dimension) map { c =>
					aux(len-1, states.flatMap(s => transitions.getOrElse((s, c), Set.empty[Int]))(breakOut))
				})
				buffer(states) = res
				res
			}
		})

		for ((from, to, chars) <- edges; char <- chars)
			transitions.getOrElseUpdate((from, char), mutable.Set()) += to

		aux(length, Set(start))
	}

}

final class MasterAutomaton private(val dimension: Int) { self =>

	require(dimension >= 0)

	sealed trait State {

		final def automaton: self.type = self

		protected[afl] def id: Int
		def length: Int

		/**
		 * The set of all words accepted by this automaton.
		 * @return a list of lists of length `self.dimension` containing lists
		 *         of length `this.length`
		 */
		def words: List[List[List[Boolean]]]

		final def padTo(newLength: Int): State =
			(this /: (length until newLength)) { case (s, i) =>
				val empty = EmptySet ofLength i
				Succ(s :: List.fill((1 << dimension) - 1)(empty))
			}

		private def binaryOp(that: State, onEmptySet: State => State, onEpsilon: => State): State = {
			require(this.length == that.length)
			val buffer = mutable.Map[(State, State), State]()

			def aux(s1: State, s2: State): State = {
				assert(this.length == that.length)

				buffer.getOrElseUpdate((s1, s2),
					buffer.getOrElse((s2, s1), ((s1, s2): @unchecked) match {
						case (EmptySet, x) => onEmptySet(x)
						case (x, EmptySet) => onEmptySet(x)
						case (Epsilon, Epsilon) => onEpsilon
						case (s1: Succ, s2: Succ) => Succ(s1.succs zip s2.succs map { t => aux(t._1, t._2) })
					})
				)
			}

			aux(this, that)
		}

		@inline final def intersect(that: State): State = binaryOp(that, _ => EmptySet, Epsilon)

		@inline final def union(that: State): State = binaryOp(that, identity, Epsilon)

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

		final def product(that: MasterAutomaton#State): MasterAutomaton#State = {
			require(this.length == that.length)

			type State = MasterAutomaton#State
			type Succ = MasterAutomaton#Succ
			val master = MasterAutomaton(that.automaton.dimension + dimension)
			val thatMaster = that.automaton
			
			val buffer = mutable.Map[(State, State), master.State]()

			def aux(s1: State, s2: State): master.State = {
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
							for (succ1 <- s1.asInstanceOf[Succ].succs; succ2 <- s2.asInstanceOf[Succ].succs)
							yield aux(succ1, succ2)
						)
				)
			}

			aux(this, that)
		}

		final def join(that: MasterAutomaton#State): MasterAutomaton#State = {
			require(this.length == that.length)
			require(dimension > 0)
			require(that.automaton.dimension > 0)
			require(dimension + that.automaton.dimension > 2)

			type State = MasterAutomaton#State
			type Succ = MasterAutomaton#Succ
			val master = MasterAutomaton(that.automaton.dimension + dimension - 2)
			val thatMaster = that.automaton
			
			val buffer = mutable.Map[(State, State), master.State]()

			def aux(s1: State, s2: State): master.State = {
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
						val succs2 = s2.asInstanceOf[Succ].succs

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

			aux(this, that)
		}

		final def section(that: MasterAutomaton#State, pos: Int): MasterAutomaton#State = {
			require(this.length == that.length)
			require(0 < pos && pos <= length)
			require(dimension > 0)

			type State = MasterAutomaton#State
			type Succ = MasterAutomaton#Succ
			val master = MasterAutomaton(dimension - 1)
			val thatMaster = that.automaton
			
			val buffer = mutable.Map[(State, State), master.State]()

			def aux(s1: State, s2: State): master.State = {
				assert(s1.length == s2.length)
				
				// The pairing strategy is almost the same as in `projection`.
				def groupedUnion(pos: Int, succs: List[State], succFalse: State, succTrue: State): List[master.State] = {
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
						val List(succFalse, succTrue) = s2.asInstanceOf[Succ].succs
						master.Succ(groupedUnion(pos, succs, succFalse, succTrue))
					}
				)
			}

			if (that.automaton.dimension != 1) {
				log("Warning: join where m(v) ≠ 1")
				master.EmptySet ofLength length
			}
			else {
				aux(this, that)
			}
		}

		final def projection(pos: Int): MasterAutomaton#State = {
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

	object EmptySet extends State {
		override def toString = "<empty>"

		protected[afl] val id = 0
		val length = 0
		val words = List.empty[List[List[Boolean]]]

		def ofLength(length: Int): State = ((this: State) /: (1 to length)) { case (s, _) => Succ(List.fill(1 << dimension)(s)) }
	}
	
	object Epsilon extends State {
		override def toString = "<epsilon>"

		protected[afl] val id = 1
		val length = 0
		val words = List(List.fill(dimension)(List.empty[Boolean]))
	}
	
	final class Succ private(protected[afl] val id: Int, val succs: List[State]) extends State {

		// the number of successors must equal 2 ^ `self.dimension`
		require(succs.length == 1 << dimension)

		// all successors must have the same length
		require(succs.groupBy(_.length).size == 1)
		
		override def toString = id + (succs zip chars(dimension) map { case (succ, c) =>
			(c map { _ ? '1' | '0' } mkString "") + "->" + succ.id
		}).mkString(" [", ", ", "]")

		val length = // pick any successor, as they all have the same length
			succs.head.length + 1

		def words =
			succs zip chars(dimension) >>= { case (s, c) =>
				s.words map { w =>
					c zip w map { case (h, t) => h :: t }
				}
			}
	}

	object Succ {
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

