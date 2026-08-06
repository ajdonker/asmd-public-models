package u07.examples

import java.util.Random

import u07.modelling.{CTMCSimulation, DAP, DAPGrid}
import u07.modelling.CTMCSimulation.*
import u07.utils.{Grids, MSet}

object DAPGossip:
  enum Place:
    case A,B,C
  type ID = (Int, Int)
  export Place.*
  export u07.modelling.DAP.*
  export u07.modelling.DAPGrid.*
  export u07.modelling.CTMCSimulation.*

  val gossipRules = DAP[Place](
    Rule(MSet(A,A), m => 1000,  MSet(A),  MSet()),   // a|a --1000--> a
    Rule(MSet(A),   m => 1,     MSet(A),  MSet(A, C)),       // a --1--> a|^a   c added to inhibit spread of a if alr reply coming
    // when request reaches target B send a reply C
    Rule(MSet(A,B), m => 1000, MSet(B, C), MSet()),
    // remove duplicate replies
    Rule(MSet(C,C), m => 1000, MSet(C), MSet()),
    // gossip reply
    Rule(MSet(C), m => 1, MSet(C), MSet(C))
  )
  val gossipCTMC = DAP.toCTMC[ID, Place](gossipRules)
  val net = Grids.createRectangularGrid(3, 3)
  // an `a` initial on top LEFT and b on the bottom right
  val state = State[ID, Place](MSet(Token((0, 0), A), Token((4, 4), B)), MSet(), net)

@main def mainDAPGossip =
  import DAPGossip.*
  gossipCTMC.newSimulationTrace(state,new Random).take(100).zipWithIndex
  .foreach:
    case (event, index) =>
      println(s"\n===== Step $index | time = ${event.time} =====")

      println("Requests (A):")
      println(DAPGrid.simpleGridStateToString[Place](event.state, A))

      println("Replies (C):")
      println(DAPGrid.simpleGridStateToString[Place](event.state, C))