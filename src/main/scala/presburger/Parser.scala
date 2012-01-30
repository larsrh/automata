package edu.tum.cs.afl.presburger

import collection.breakOut
import util.control.Exception._

import org.antlr.runtime._
import org.antlr.runtime.tree._

import edu.tum.cs.afl.antlr._
import edu.tum.cs.afl.Util._

/**
 * Umbrella object for the parser. Contains utility method which traverse the
 * AST obtained from ANTLR.
 */
object Parser {

	def child(ast: CommonTree, pos: Int) = ast.getChild(pos).asInstanceOf[CommonTree]

	def getText(ast: CommonTree) = {
		assert(ast.getToken().getType() == PA2Parser.VAR)
		ast.getText()
	}

	def getNum(ast: CommonTree) = {
		val tpe = ast.getToken().getType()
		assert(tpe == PA2Parser.PLUS || tpe == PA2Parser.MINUS)

		val c = child(ast, 0)
		assert(c.getToken().getType() == PA2Parser.INT)

		val num = BigInt(c.getText())

		tpe match {
			case PA2Parser.PLUS => num
			case PA2Parser.MINUS => -num
		}
	}

	def collectChildren(from: Int, ast: CommonTree): List[CommonTree] =
		(from until ast.getChildCount()).map(child(ast, _))(breakOut)
	
	def convertSummand(ast: CommonTree): (BigInt, String) =
		(getNum(ast), getText(child(ast, 1)))
	
	val IsRelation = MapExtractor(Map(
		PA2Parser.EQ -> Relation.Equal,
		PA2Parser.GEQ -> Relation.GreaterOrEqual,
		PA2Parser.GT -> Relation.Greater,
		PA2Parser.LEQ -> Relation.LessOrEqual,
		PA2Parser.LT -> Relation.Less,
		PA2Parser.NEQ -> Relation.NotEqual
	))

	val IsBinary = MapExtractor(Map(
		PA2Parser.AND -> And.apply _,
		PA2Parser.EQV -> Iff.apply _,
		PA2Parser.IMP -> Implies.apply _,
		PA2Parser.OR -> Or.apply _
	))

	def convertFormula(ast: CommonTree): Formula = ast.getToken().getType() match {
		case PA2Parser.ALL =>
			Forall(getText(child(ast, 0)), convertFormula(child(ast, 1)))
		case PA2Parser.EX =>
			Exists(getText(child(ast, 0)), convertFormula(child(ast, 1)))

		case PA2Parser.NEG =>
			Not(convertFormula(child(ast, 0)))

		case IsBinary(op) =>
			op(convertFormula(child(ast, 0)), convertFormula(child(ast, 1)))

		case IsRelation(rel) =>
			Relation(collectChildren(1, ast) map convertSummand, getNum(child(ast, 0)), rel)
	}

	def parse(contents: String): Option[Formula] =
		catching(classOf[NullPointerException]) opt {
			convertFormula(
				new PA2Parser(
					new CommonTokenStream(new PA2Lexer(new ANTLRStringStream(contents)))
				).formula().getTree().asInstanceOf[CommonTree]
			)
		}

}

