package edu.tum.cs.afl

import scalaz._
import Scalaz._

import MasterAutomaton._
import Util._

/** Entry point for the application */
object Launcher extends App {

	def usage() {
		Console.err println "Usage: ((--program | --presburger <length> [y|n]) <file>*)*"
	}

	type Consumer = List[String] =>? Unit

	def switchToOr(pf: Consumer) = switchTo orElse pf orElse doNothing

	def processProgram: Consumer = switchToOr {
		case file :: tail =>
			val result = for (
				parsed <- programs.Parser.parseProgram(readFile(file));
				automaton <- parsed.executeAndGet toSuccess "program runtime error"
			) yield	writeFile(file + ".txt", automaton.words map wordToIntWord)

			result.fail.toOption foreach { msg => Console.err println ("Error in file " + file + ": " + msg) }

			processProgram(tail)
	}

	def processPresburger(length: Int, output: Boolean) = switchToOr {
		case file :: tail =>
			sys error "not implemented yet"
	}

	def switchTo: Consumer = {
		case "--program" :: tail => processProgram(tail)
		case "--presburger" :: num :: tail =>
			val n = num.toInt
			tail match {
				case "y" :: rest => processPresburger(n, true)(rest)
				case "n" :: rest => processPresburger(n, false)(rest)
				case rest => processPresburger(n, false)(rest)
			}
		case "--presburger" :: _ =>
			Console.err println "Not enough parameters for --presburger"
			usage()
	}

	def doNothing: Consumer = {
		case Nil =>
	}


	if (args.length < 1)
		usage()
	
	switchTo lift args.toList getOrElse usage()

}
