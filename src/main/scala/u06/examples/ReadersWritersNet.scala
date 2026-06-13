package u06.examples

import u06.modelling.PetriNet
import u06.modelling.PetriNet.*
import u06.utils.MSet.*

object ReadersWritersNet:

  enum Place:
    case P1,P2,P3,P4,P5,P6,P7

//  P8 used as waiting-to-read state
  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet
  private val coreTransitions: Set[Trn[Place]] = PetriNet(
    MSet(P1) ~~> MSet(P2),

    MSet(P2) ~~> MSet(P3),

    MSet(P2) ~~> MSet(P4),

    MSet(P3,P5) ~~> MSet(P5,P6),

    MSet(P4,P5) ~~> MSet(P7) ^^^ MSet(P6)
  )
  private val eventualReadCoreTransitions: Set[Trn[Place]] = PetriNet(
    MSet(P1) ~~> MSet(P2),

    MSet(P2) ~~> MSet(P3),

    MSet(P2) ~~> MSet(P4),

    MSet(P3, P5) ~~> MSet(P5, P6),

    MSet(P4, P5) ~~> MSet(P7) ^^^ MSet(P6, P3)
  )
  private val loopTransitions: Set[Trn[Place]] = PetriNet(
    MSet(P6) ~~> MSet(P1),

    MSet(P7) ~~> MSet(P1,P5)
  )
  val RW = (coreTransitions ++ loopTransitions).toSystem

  val rwAcyclic = (coreTransitions ++ PetriNet(MSet(P7) ~~> MSet(P5))).toSystem
  val eventualReadRW = (eventualReadCoreTransitions ++ loopTransitions).toSystem
  def init(k: Int) = MSet.ofList(List.fill(k)(P1) :+ P5)

@main def mainReadersWriters =
  import ReadersWritersNet.*
  println(RW.paths(MSet(P1,P1,P1,P1,P1,P5), 7).toList.mkString("\n"))