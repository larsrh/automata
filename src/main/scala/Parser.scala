package edu.tum.cs.afl

import util.parsing.combinator.RegexParsers
import util.matching.Regex

object Parser {

	import Program._

	trait Commons extends RegexParsers {

		def number: Parser[Int] = """[0-9]+\s*""".r ^^ {s => s.trim.toInt}

		final def parseAll[T](parser: this.type => Parser[T], input: String): Either[String, T] = parseAll(parser(this), input) match {
			case NoSuccess(msg, _) => Left(msg)
			case Success(result, _) => Right(result)
		}

	}

	object ProgramParser extends Commons {

		def program: Parser[Program] = rep1(command) ^^ { cmds => Program(cmds) }

		def command: Parser[Command] = (variable <~ """=\s*""".r) ~ expression <~ """;\s*""".r ^^ { case v ~ e => (v, e) }

		def expression: Parser[Expression] = expressions reduce { _ | _ }

		def expressions: List[Parser[Expression]] = List(
			expr("union", variable ~ variable)            ^^ { case v1 ~ v2 => Union(v1, v2) },
			expr("intersection", variable ~ variable)     ^^ { case v1 ~ v2 => Intersection(v1, v2) },
			expr("negation", variable)                    ^^ { v => Negation(v) },
			expr("join", variable ~ variable)             ^^ { case v1 ~ v2 => Product(v1, v2) },
			expr("product", variable ~ variable)          ^^ { case v1 ~ v2 => Product(v1, v2) },
			expr("project", variable ~ number)            ^^ { case v ~ n => Project(v, n) },
			expr("section", variable ~ number ~ variable) ^^ { case v1 ~ n ~ v2 => Section(v1, n, v2) },
			expr("read", file)                            ^^ { f => Read(f) }
		)

		def variable: Parser[Variable] = """[a-z]\s*""".r ^^ {s => Variable(s.trim charAt 0)}
		def file: Parser[String] = """[a-zA-Z0-9]*\s*""".r ^^ {_.trim}

		def expr[T](keyword: String, parser: Parser[T]) = keyword ~ """\s*""".r ~> parser

	}

	object AutomatonParser extends Commons {

		import Function._

		def automaton: Parser[MasterAutomaton#State] =
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
					MasterAutomaton.fromEdges(start, edges, end, length, dim)
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
