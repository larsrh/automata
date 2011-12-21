package edu.tum.cs.afl

import io.Source

import scalaz._
import Scalaz._

import MasterAutomaton._
import Util._

object Program {

	case class Program(cmds: List[Command]) {
		private def cmdK(cmd: Command) = kleisli((env: Environment) => env execute cmd)

		private def mkKleisli = cmds.toNel map { nel => 
			(cmdK(nel.head) /: nel.tail) { case (acc, cmd) => acc >=> cmdK(cmd) }
		} getOrElse {
			log("Warning: empty command list")
			kleisli((env: Environment) => env.some)
		}

		def execute = mkKleisli(new Environment())
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

	class Environment(vars: Map[Char, Automaton]) {
		def this() = this(Map.empty)

		def apply(v: Variable) = vars get v.c orElse {
			log("Warning: variable not existent " + v.c)
			None
		}

		def +(binding: (Variable, Automaton)) = {
			val (Variable(c), a) = binding

			if (vars contains c)
				log("Warning: re-binding variable " + c)

			new Environment(vars.updated(c, a))
		}

		def execute(cmd: Command): Option[Environment] = {
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

			res map { r => this + ((v, r)) }
		}
	}

}

