package edu.tum.cs.afl.programs

import scalaz._
import Scalaz._

import edu.tum.cs.afl.Util._

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

