package u06.modelling

import u06.utils.MSet
import u06.modelling.PetriNet

import scala.language.postfixOps
object PrioPetriNet {
    case class PrioTrn[P](
                         cond: MSet[P],
                         eff: MSet[P],
                         inh: MSet[P],
                         priority: Int
                         )
    type PrioPetriNet[P] = Set[PrioTrn[P]]
    type Marking[P] = MSet[P]
    extension [P](ppn: PrioPetriNet[P])
      def toSystem: System[Marking[P]] = m => 
        val enabledTransitions = ppn.filter(t => (m.disjoined(t.inh)) && (m.matches(t.cond)))
        val maxPriority = enabledTransitions.map(_.priority).maxOption.getOrElse(-1)
        for
          t <- enabledTransitions
          prio = t.priority
          if prio == maxPriority && maxPriority >= 0
          out <- m.extract(t.cond)
        yield out.union(t.eff)

    extension [P](trn: PetriNet.Trn[P])
      def priority(p: Int): PrioTrn[P] =
        PrioTrn(trn.cond, trn.eff, trn.inh, p)
}

