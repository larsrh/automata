package edu.tum.cs.afl.presburger

import collection.mutable

import scalaz._
import Scalaz._

import edu.tum.cs.afl.MasterAutomaton
import MasterAutomaton._
import edu.tum.cs.afl.Util._

object Printer {

	def automatonToDotty(automaton: Automaton): List[String] = {
		def aux(state: Automaton): List[String] = {
			if (state == state.automaton.EmptySet)
				Nil
			else if (state == state.automaton.Epsilon)
				List(state.id + "[peripheries=2];")
			else
				state.asInstanceOf[MasterAutomaton#Succ].succs zip chars(automaton.automaton.dimension) flatMap { case (s, c) =>
					if (s.empty)
						Nil
					else {
						val head = state.id + " -> " + s.id + " [label=\"" + (c map { elem => elem ? '1' | '0' } mkString "")  +  " \"];"
						val tail = aux(s)
						head :: tail
					}
				}
		}

		val init = automaton.id + "[shape=diamond];"

		"digraph G {" :: init :: (aux(automaton) :+ "}")
	}

}

