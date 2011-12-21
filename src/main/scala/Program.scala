package edu.tum.cs.afl

import io.Source

import scalaz._
import Scalaz._

import MasterAutomaton._
import Util._

/**
 * Umbrella object for the representation and execution of programs and
 * commands.
 */
object Program {

	/**
	 * A program, consisting of a non-empty list of `Command`s.
	 *
	 * A program can be obtained via `Parser.parseProgram`.
	 *
	 * You might wish to use the `executeAndGet` method.
	 */
	case class Program(cmds: List[Command]) {
		// I tried using `Kleisli` here, but guess what, all those type annotations
		// made it cumbersome.

		/**
		 * Type of a single optional state transition function, also carrying the
		 * variable which has been assigned during this transition.
		 *
		 * ("state" refers to `Environment`, not to a state of an automaton)
		 */
		private type CmdF = Environment => Option[(Variable, Environment)]

		/** Converts a `Command` to an optional state transition function. */
		@inline private def mkCmd(cmd: Command): CmdF = (_ execute cmd)

		/** Composes a `Command` and an optional state transition function. */
		@inline private def andThen: (CmdF, Command) => CmdF = { case (f, cmd) =>
			f andThen { _ flatMap { case (_, env) => mkCmd(cmd)(env) } }
		}

		/** Optionally composes a list of `Command`s. **/
		@inline private def mkCmds = cmds.toNel map {
			nel => (mkCmd(nel.head) /: nel.tail)(andThen)
		} orElse {
			log("Error: empty command list")
			None
		}

		/** Optionally executes a list of `Command`s on an empty `Environment`. */
		@inline def execute = mkCmds flatMap { _(new Environment()) }

		/**
		 * Optionally executes a list of `Command`s on an empty `Environment`
		 * and retrieves the value of the variable which has been assigned in
		 * the last command.
		 */
		@inline def executeAndGet = execute flatMap { case (v, env) => env(v) }
	}

	type Command = (Variable, Expression)

	case class Variable(c: Char) {
		require('a' to 'z' contains c)
	}

	sealed trait Expression

	case class Union(v1: Variable, v2: Variable) extends Expression
	case class Intersection(v1: Variable, v2: Variable) extends Expression
	case class Negation(v: Variable) extends Expression
	case class Join(v1: Variable, v2: Variable) extends Expression
	case class Product(v1: Variable, v2: Variable) extends Expression
	case class Project(v: Variable, n: Int) extends Expression
	case class Section(v1: Variable, n: Int, v2: Variable) extends Expression
	case class Read(f: String) extends Expression {
		lazy val file = new java.io.File(f)
	}

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

}

