package scala.u06.modelling

import u06.modelling.PetriNet
import u06.modelling.PrioPetriNet
import u06.modelling.PrioPetriNet.PrioTrn
import u06.modelling.PrioPetriNet.toSystem
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import u06.utils.MSet

class PrioPetriNetSpec extends AnyFunSuite:

  val high = PrioTrn(MSet("A"), MSet("B"), MSet(), 1)
  val low = PrioTrn(MSet("A"), MSet("C"), MSet(), 0)

  test("higher-priority transition fires instead of lower-priority transition"):
    val ppn = Set(high, low)
    val system = ppn.toSystem

    system.next(MSet("A")).should(contain.only(MSet("B")))

  test("extra tokens are preserved after firing"):
    val ppn = Set(high, low)
    val system = ppn.toSystem

    system.next(MSet("A", "D")).should(contain.only(MSet("B", "D")))
    system.next(MSet("A", "E")).should(contain.only(MSet("B", "E")))

  test("lower-priority transition fires when higher-priority one is inhibited"):
    val high = PrioTrn(MSet("A"), MSet("B"), MSet("X"), 1)
    val low = PrioTrn(MSet("A"), MSet("C"), MSet(), 0)

    val ppn = Set(high, low)
    val system = ppn.toSystem

    system.next(MSet("A", "X")).shouldBe(Set(MSet("X", "C")))

  test("transitions with the same priority can both fire"):
    val ppn = Set(
      PrioTrn(MSet("A"), MSet("B"), MSet(), 1),
      PrioTrn(MSet("A"), MSet("C"), MSet(), 1)
    )
    val system = ppn.toSystem

    system.next(MSet("A")).shouldBe(Set(MSet("B"), MSet("C")))

  test("no transition fires if none is enabled"):
    val ppn = Set(high, low)
    val system = ppn.toSystem

    system.next(MSet("D")).shouldBe(Set.empty)

  test("inhibitor prevents a transition from firing"):
    val inhibited = PrioTrn(MSet("A"), MSet("B"), MSet("X"), 1)
    val ppn = Set(inhibited)
    val system = ppn.toSystem

    system.next(MSet("A", "X")).shouldBe(Set.empty)
