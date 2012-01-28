package edu.tum.cs.afl.programs

import collection.{mutable, breakOut}
import util.parsing.combinator.RegexParsers
import util.matching.Regex

import scalaz._
import Scalaz._

import edu.tum.cs.afl.MasterAutomaton
import edu.tum.cs.afl.Util

/**
 * Umbrella object for the parsers, based on the Scala combinator parsing
 * library.
 */
object Parser {

	import Program._

	/** Utilities. */
	trait Commons extends RegexParsers {

		/** Parses a natural number followed by an arbitrary number of whitespace characters. */
		def number: Parser[Int] = """[0-9]+\s*""".r ^^ {s => s.trim.toInt}

		/**
		 * Invokes the specified parser and transforms the parse result to an
		 * object containing either an error message or the output.
		 */
		final def parseAll[T](parser: this.type => Parser[T], input: String): Validation[String, T] = parseAll(parser(this), input) match {
			case NoSuccess(msg, _) => msg.fail
			case Success(result, _) => result.success
		}

	}

	/** The parser for programs. **/
	object ProgramParser extends Commons {

		def program: Parser[Program] = rep1(command) ^^ { cmds => Program(cmds) }

		def command: Parser[Command] = (variable <~ """=\s*""".r) ~ expression <~ """;\s*""".r ^^ { case v ~ e => (v, e) }

		def expression: Parser[Expression] = expressions reduce { _ | _ }

		def expressions: List[Parser[Expression]] = List(
			expr("union", variable ~ variable)            ^^ { case v1 ~ v2 => Union(v1, v2) },
			expr("intersection", variable ~ variable)     ^^ { case v1 ~ v2 => Intersection(v1, v2) },
			expr("negation", variable)                    ^^ { v => Negation(v) },
			expr("join", variable ~ variable)             ^^ { case v1 ~ v2 => Join(v1, v2) },
			expr("product", variable ~ variable)          ^^ { case v1 ~ v2 => Product(v1, v2) },
			expr("project", variable ~ number)            ^^ { case v ~ n => Project(v, n) },
			expr("section", variable ~ number ~ variable) ^^ { case v1 ~ n ~ v2 => Section(v1, n, v2) },
			expr("read", file)                            ^^ { f => Read(f) }
		)

		def variable: Parser[Variable] = """[a-z]\s*""".r ^^ {s => Variable(s.trim charAt 0)}
		def file: Parser[String] = """[a-zA-Z0-9]*\s*""".r ^^ {_.trim}

		def expr[T](keyword: String, parser: Parser[T]) = keyword ~ """\s*""".r ~> parser

	}

	/** The parser for automata. **/
	object AutomatonParser extends Commons {

		import MasterAutomaton._
		import Function._

		/**
		 * Produces a state corresponding to `start` in the `MasterAutomaton`
		 * of the specified dimension. May fail if `start` does not accept a
		 * fixed-length language.
		 * @param dimension must be greater than 0
		 * @param length must be non-negative
		 */
		def fromEdges(start: Int, edges: Seq[(Int, Int, Seq[Seq[Boolean]])], end: Int, length: Int, dimension: Int): Automaton = {
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
					val res = master.Succ(Util.chars(dimension) map { c =>
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

		def automaton: Parser[Automaton] =
			number >> { dim => 
				number ~
				"""digraph G \{\s*""".r ~
				state ~ state ~
				rep1(edge(dim)) ~
				"""\}\s*""".r ^^ { case length ~ _ ~ start ~ end ~ edges ~ _ =>
					Util.log("dimension: " + dim)
					Util.log("length: " + length)
					Util.log("start: " + start)
					Util.log("end: " + end)
					Util.log("edges: " + edges)
					fromEdges(start, edges, end, length, dim)
				}
			}


		def state: Parser[Int] = number <~ """\[.*\];\s*""".r

		def edge(dimension: Int): Parser[(Int, Int, Seq[Seq[Boolean]])] =
			number ~ """->\s*""".r ~ number ~ "[label=\"" ~ label(dimension) ~ """\"];\s*""".r ^^
			{ case from ~ _ ~ to ~ _ ~ label ~ _ => (from, to, label) }

		def boolean: Parser[Boolean] = ("0" ^^ const(false)) | ("1" ^^ const(true))

		def label(dimension: Int): Parser[Seq[Seq[Boolean]]] = rep1(repN(dimension, boolean) <~ """\s*""".r)

	}

	def parseProgram(s: String) = ProgramParser.parseAll(_.program, s)
	
	def parseAutomaton(s: String) = AutomatonParser.parseAll(_.automaton, s)

}
