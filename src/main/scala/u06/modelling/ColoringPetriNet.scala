package u06.modelling
import u06.utils.MSet

object ColoringPetriNet {
  case class CToken[P, C](place: P, color: C)

  case class ColoringTrn[P, C](
                                cond: MSet[CToken[P, C]], // required token and color to fire
                                eff: MSet[CToken[P, C]] => MSet[CToken[P, C]], // functions that produce outgoing tokens with colors based on the colors of the incoming tokens
                                inh: MSet[CToken[P, C]],
                              )

  type ColoringPetriNet[P, C] = Set[ColoringTrn[P, C]]
  type Marking[P, C] = MSet[CToken[P, C]]
  extension [P, C](cpn: ColoringPetriNet[P, C])
    def toSystem: System[Marking[P, C]] = m =>
      for
        t <- cpn
        if m.disjoined(t.inh) && m.matches(t.cond)
        out <- m.extract(t.cond)
      yield out.union(t.eff(t.cond))

}