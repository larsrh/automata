package edu.tum.cs.afp

object Data {

	type Program = List[Command]

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

}

