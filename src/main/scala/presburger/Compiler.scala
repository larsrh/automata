package edu.tum.cs.afl.presburger

import collection.{mutable, SortedSet}

import scalaz._
import Scalaz._

import edu.tum.cs.afl.MasterAutomaton
import MasterAutomaton._
import edu.tum.cs.afl.Util._

object Compiler {

	type MetaAutomaton = (Automaton, Seq[String])

	def evaluate(f: Formula, length: Int): (Seq[Seq[BigInt]], Seq[String]) = {
		val compiler = new Compiler(length)
		val (automaton, vars) = compiler compile f
		val words = automaton.words map { _ map seqToBigInt }
		(words, vars)
	}

}

final class Compiler(length: Int) {

	import Compiler._

	def compileRelation(weights: Seq[BigInt], accept: BigInt => Boolean): Automaton = {
		require(weights.length > 0)

		val master = MasterAutomaton(weights.length)
		val buffer = mutable.Map[BigInt, master.State]()

		def nextVal(sum: BigInt, bits: Seq[Boolean], factor: BigInt): BigInt = {
			assert(bits.length == weights.length)

			val summands = bits zip weights map { case (bit, weight) =>
				bit ? (weight * factor) | BigInt(0)
			}

			sum + summands.sum
		}

		def endState(accept: Boolean) = if (accept) master.Epsilon else master.EmptySet

		def aux(sum: BigInt, factor: BigInt, remaining: Int): master.State = {
			val sums = chars(weights.length) map { nextVal(sum, _, factor) }

			if (remaining == 0)
				master.Succ(sums map { sum => endState(accept(sum)) })
			else
				master.Succ(sums map { sum => aux(sum, factor >> 1, remaining - 1) })
		}

		aux(0, BigInt(1) << (length - 1), length - 1)
	}

	def equalize(ma1: MetaAutomaton, ma2: MetaAutomaton) = {
		def ensureVar(a: Automaton, vars: Seq[String], v: String, pos: Int) = vars match {
			case Seq(`v`, tail @ _*) => (a, tail)
			case _ => (a insert pos, vars)
		}

		def aux(a1: Automaton, vars1: Seq[String], a2: Automaton, vars2: Seq[String], vars: Seq[String], pos: Int): (Automaton, Automaton) =
			vars match {
				case Seq() => (a1, a2)

				case Seq(head, tail @ _*) =>
					val (a1v, v1v) = ensureVar(a1, vars1, head, pos)
					val (a2v, v2v) = ensureVar(a2, vars2, head, pos)
					aux(a1v, v1v, a2v, v2v, tail, pos + 1)
			}

		val (a1, vars1) = ma1
		val (a2, vars2) = ma2

		val allVars = SortedSet(vars1 ++ vars2: _*).toSeq

		(aux(a1, vars1, a2, vars2, allVars, 0), allVars)
	}

	def compile(f: Formula): MetaAutomaton = {
		println("compile("+f+")")
		val res = f match {

		case Not(f) =>
			val (a, vars) = compile(f)
			(a.complement, vars)

		case And(f1, f2) => 
			val ((a1, a2), vars) = equalize(compile(f1), compile(f2))
			(a1 intersect a2, vars)

		case Or(f1, f2) =>
			val ((a1, a2), vars) = equalize(compile(f1), compile(f2))
			(a1 union a2, vars)

		case rel @ Relation(weights, vars, _, _) =>
			(compileRelation(weights, rel.acceptor), vars.toSeq)

		case Exists(v, f) =>
			val (a, vars) = compile(f)
			vars indexOf v match {
				case -1 => (a, vars)
				case n => (a projection (n + 1), vars.patch(n, Seq.empty, 1))
			}

		case Forall(v, f) =>
			compile(Not(Exists(v, Not(f))))

	}
		println("res of compile("+f+") = " + res)
		res
}
}
