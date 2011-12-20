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

				buffer.getOrElseUpdate((s1, s2), {
					buffer.getOrElse((s2, s1), (s1, s2) match {
						case (EmptySet, x) => onEmptySet(x)
						case (x, EmptySet) => onEmptySet(x)
						case (Epsilon, Epsilon) => onEpsilon
						case (s1: Succ, s2: Succ) => Succ(s1.succs zip s2.succs map { t => aux(t._1, t._2) })
					})
				})
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

