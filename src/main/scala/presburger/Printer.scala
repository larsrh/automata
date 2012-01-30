package edu.tum.cs.afl.presburger

import collection.mutable

import scalaz._
import Scalaz._

import edu.tum.cs.afl.MasterAutomaton
import MasterAutomaton._
import edu.tum.cs.afl.Util._

object Printer {

	def automatonToDotty(automaton: Automaton): List[String] = {
		val visited = mutable.Set[Automaton]()

		def aux(state: Automaton): List[String] =
			if (visited contains state)
				Nil
			else {
				visited += state

				if (state == state.automaton.EmptySet || state == state.automaton.Epsilon)
					Nil
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
		val end = if (!automaton.empty) "1[peripheries=2];" else ""

		"digraph G {" :: init :: end :: (aux(automaton) :+ "}")
	}

}

