package edu.tum.cs.afl

import util.parsing.combinator.RegexParsers
import util.matching.Regex

object Parser {

	import Program._

	object ProgramParser extends RegexParsers {

		def program: Parser[Program] = rep1(command)

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
		def number: Parser[Int] = """[0-9]*\s*""".r ^^ {s => s.trim.toInt}
		def file: Parser[String] = """[a-zA-Z0-9]*\s*""".r ^^ {_.trim}

		def expr[T](keyword: String, parser: Parser[T]) = keyword ~ """\s*""".r ~> parser

	}

	def parse(s: String): Program = ProgramParser.parseAll(ProgramParser.program, s).get

}
