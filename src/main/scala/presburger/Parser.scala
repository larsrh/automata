package edu.tum.cs.afl.presburger

import org.antlr.runtime._
import org.antlr.runtime.tree._

import edu.tum.cs.afl.antlr._

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

	def collectChildren(from: Int, ast: CommonTree): Seq[CommonTree] =
		from until ast.getChildCount() map { child(ast, _) }
	
	def convertSummand(ast: CommonTree): (BigInt, String) =
		(getNum(ast), getText(child(ast, 1)))

	def convertFormula(ast: CommonTree): Formula = ast.getToken().getType() match {
		case PA2Parser.ALL =>
			Forall(getText(child(ast, 0)), convertFormula(child(ast, 1)))
		case PA2Parser.EX =>
			Exists(getText(child(ast, 0)), convertFormula(child(ast, 1)))

		case PA2Parser.AND =>
			And(convertFormula(child(ast, 0)), convertFormula(child(ast, 1)))
		case PA2Parser.EQV =>
			Iff(convertFormula(child(ast, 0)), convertFormula(child(ast, 1)))
		case PA2Parser.IMP =>
			Implies(convertFormula(child(ast, 0)), convertFormula(child(ast, 1)))
		case PA2Parser.OR =>
			Or(convertFormula(child(ast, 0)), convertFormula(child(ast, 1)))

		case PA2Parser.NEG =>
			Not(convertFormula(child(ast, 0)))

		case PA2Parser.EQ =>
			Relation(collectChildren(1, ast) map convertSummand, getNum(child(ast, 0)), Relation.Equal)
		case PA2Parser.GEQ =>
			Relation(collectChildren(1, ast) map convertSummand, getNum(child(ast, 0)), Relation.GreaterOrEqual)
		case PA2Parser.GT =>
			Relation(collectChildren(1, ast) map convertSummand, getNum(child(ast, 0)), Relation.Greater)
		case PA2Parser.LEQ =>
			Relation(collectChildren(1, ast) map convertSummand, getNum(child(ast, 0)), Relation.LessOrEqual)
		case PA2Parser.LT =>
			Relation(collectChildren(1, ast) map convertSummand, getNum(child(ast, 0)), Relation.Less)
		case PA2Parser.NEQ =>
			Relation(collectChildren(1, ast) map convertSummand, getNum(child(ast, 0)), Relation.NotEqual)
	}

	def parse(contents: String): Formula = convertFormula(
		new PA2Parser(
			new CommonTokenStream(new PA2Lexer(new ANTLRStringStream(contents)))
		).formula().getTree().asInstanceOf[CommonTree]
	)

}

