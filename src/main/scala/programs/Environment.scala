package edu.tum.cs.afl.programs

import io.Source

import scalaz._
import Scalaz._

import edu.tum.cs.afl.MasterAutomaton.Automaton
import edu.tum.cs.afl.Util._

/**
 * An `Environment` represents a mapping between variable (names) and
 * automata. Internally, this is represented as a `Map` from `Char` to
 * `Automaton`.
 *
 * An automaton is "assigned" to a variable in this context, and
 * a variable might be "unassigned".
 */
class Environment(vars: Map[Char, Automaton]) {

	/** Creates an empty environment. */
	def this() = this(Map.empty)

	/** 
	 * Optionally retrieves the automaton assigned to the specified variable.
	 * Side effects: logs a warning if the desired variable is unassigned.
	 */
	def apply(v: Variable) = vars get v.c orElse {
		log("Warning: variable is unassigned " + v.c)
		None
	}

	/**
	 * Adds an assignment to this `Environment`.
	 * Side effects: logs a warning if the desired variable is already
	 * assigned.
	 *
	 * Already assigned variables are overwritten with the new
	 * assignment.
	 */
	def +(assignment: (Variable, Automaton)) = {
		val (Variable(c), a) = assignment

		if (vars contains c)
			log("Warning: re-assigning variable " + c)

		new Environment(vars.updated(c, a))
	}

	/**
	 * Applies the command `cmd` to this environment, and optionally
	 * returns a pair of the last assigned variable (guaranteed to be
	 * equal to `cmd._1`) and the updated environment.
	 */
	def execute(cmd: Command): Option[(Variable, Environment)] = {
		log("Executing " + cmd)

		val (v, expr) = cmd

		val res = expr match {
			case Union(v1, v2) =>
				(this(v1) |@| this(v2)) { _ union _ }

			case Intersection(v1, v2) =>
				(this(v1) |@| this(v2)) { _ intersect _ }

			case Negation(v) =>
				this(v) map { _.complement }

			case Join(v1, v2) =>
				(this(v1) |@| this(v2)) { _ join _ }

			case Product(v1, v2) =>
				(this(v1) |@| this(v2)) { _ product _ }

			case Project(v, n) =>
				this(v) map { _ projection n }

			case Section(v1, n, v2) =>
				(this(v1) |@| this(v2)) { _.section(_, n) }

			case Read(f) =>
				Parser.parseAutomaton(Source fromFile f mkString).fold(
					err => {
						Console.err println ("Error: parsing file " + f + " failed")
						log("Error: " + err)
						None
					},
					success =>
						Some(success)
				)
		}

		res map { r => (v, this + (v -> r)) }
	}
}
