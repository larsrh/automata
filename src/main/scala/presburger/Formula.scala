package edu.tum.cs.afl.presburger

import math._

sealed trait Formula {
	def freeVars: Set[String]

	final def isSentence = freeVars.isEmpty
}

object Relation {

	sealed trait Type
	case object Equal extends Type
	case object GreaterOrEqual extends Type
	case object Greater extends Type
	case object LessOrEqual extends Type
	case object Less extends Type
	case object NotEqual extends Type

	/** Produces a `Relation` after grouping and filtering the summands. */
	def apply(summands: List[(BigInt, String)], constant: BigInt, tpe: Type) = {
		val grouped = summands groupBy { _._2 } mapValues { _ map { _._1 } sum } filter { case (v, c) => c != BigInt(0) }
		val (vars, weights) = grouped.toList.sorted.unzip
		new Relation(weights, vars, constant, tpe)
	}

}

case class Relation private(weights: List[BigInt], vars: List[String], constant: BigInt, tpe: Relation.Type) extends Formula {

	require(weights.length == vars.length)

	import Relation._

	def freeVars = vars.toSet

	def acceptor: BigInt => Boolean = tpe match {
		case Equal =>          _ == constant
		case GreaterOrEqual => _ >= constant
		case Greater =>        _ >  constant
		case LessOrEqual =>    _ <= constant
		case Less =>           _ <  constant
		case NotEqual =>       _ != constant
	}

}

case class Not(f: Formula) extends Formula {
	def freeVars = f.freeVars
}

case class Or(f1: Formula, f2: Formula) extends Formula {
	def freeVars = f1.freeVars ++ f2.freeVars
}

case class And(f1: Formula, f2: Formula) extends Formula {
	def freeVars = f1.freeVars ++ f2.freeVars
}

case class Exists(v: String, f: Formula) extends Formula {
	def freeVars = f.freeVars - v
}

case class Forall(v: String, f: Formula) extends Formula {
	def freeVars = f.freeVars - v
}

object Implies {
	def apply(f1: Formula, f2: Formula) = Or(Not(f1), f2)
}

object Iff {
	def apply(f1: Formula, f2: Formula) = And(Implies(f1, f2), Implies(f2, f1))
}

