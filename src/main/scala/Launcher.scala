package edu.tum.cs.afl

import io.Source
import java.io.{PrintWriter, FileOutputStream}

import MasterAutomaton._
import Util._

/** Entry point for the application */
object Launcher extends App {

	if (args.length < 1)
		Console.err println "Warning: Nothing to do. Please specify at least one input file"
	
	for (f <- args) {
		try {
			val contents = Source fromFile f mkString
			val parseResult = Parser parseProgram contents

			parseResult.fold(
				err => {
					log("Error in file " + f + ": " + err)
					sys error ("parsing file " + f + " failed")
				},
				success => {
					val stream = new PrintWriter(new FileOutputStream(f + ".txt"))
					val result = success.executeAndGet
					result match {
						case None =>
							sys error "program runtime error"

						case Some(automaton) =>
							for (w <- automaton.words)
								stream println (wordToIntWord(w))
					}
					stream.flush()
					stream.close()
				}
			)
		}
		catch {
			case ex: Exception =>
				Console.err println ("Error in file " + f + ": " + ex)
		}
	}

}
