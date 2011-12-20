package edu.tum.cs.afl

import collection.mutable

import scalaz._
import Scalaz._

import Util._

object MasterAutomaton {

	private val buffer = mutable.Map[Int, MasterAutomaton]()

	def apply(dim: Int) = buffer.getOrElseUpdate(dim, new MasterAutomaton(dim))

}

final class MasterAutomaton private(val dimension: Int) { self =>

	require(dimension >= 1)

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
					// this check only makes sense if `s1` and `s2` have the same dimension
					buffer.getOrElse((s2, s1), 
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
				)
			}

			aux(this, that)
		}

		final def projection(pos: Int): MasterAutomaton#State = {
			require(0 < pos && pos <= dimension)

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

