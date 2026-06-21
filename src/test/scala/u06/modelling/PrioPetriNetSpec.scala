package scala.u06.modelling

import u06.modelling.PetriNet.Trn
import u06.modelling.PrioPetriNet
import u06.modelling.PrioPetriNet.PrioTrn
import u06.modelling.PrioPetriNet.toSystem
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import u06.utils.MSet

class PrioPetriNetSpec extends AnyFunSuite:

  val high = PrioTrn(MSet("A"), MSet("B"), MSet(), 1)
  val low = PrioTrn(MSet("A"), MSet("C"), MSet(), 0)

  test("a transition can fire if no higher priority transition is enabled"):
    val ppn = Set(high, low)
    val system = ppn.toSystem

    system(MSet("A")).should(contain.only(MSet("B"))) // high priority transition fires
    system(MSet("A", "D")).should(contain.only(MSet("B"))) // high priority transition fires, low is inhibited by D
    system(MSet("A", "E")).should(contain.only(MSet("B"))) // high priority transition fires, low is inhibited by E

  test("lower-priority transition fires when higher-priority one is not enabled"):

    val high = PrioTrn(MSet("A"), MSet("B"), MSet("X"), 1)
    val low = PrioTrn(MSet("A"), MSet("C"), MSet(), 0)

    val ppn = Set(high, low)
    val system = ppn.toSystem

    system(MSet("A", "X")).shouldBe(Set(MSet("C")))

  test("transition with the same priority can both fire"):
    val ppn = Set(
      PrioTrn(MSet("A"), MSet("B"), MSet(), 1),
      PrioTrn(MSet("A"), MSet("C"), MSet(), 1)
    )
    val system = ppn.toSystem

    system(MSet("A")).should(contain.only(MSet("B"), MSet("C"))) // both transitions can fire
