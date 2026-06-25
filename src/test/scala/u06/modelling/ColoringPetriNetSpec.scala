package scala.u06.modelling
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import u06.utils.MSet
import u06.modelling.ColoringPetriNet.*
class ColoringPetriNetSpec extends AnyFunSuite:
  test("transition consumes a colored token and produces a new colored token"):
    val redA = CToken("A", "red")
    val blueB = CToken("B", "blue")

    val trn = ColoringTrn(
      cond = MSet(redA),
      eff = _ => MSet(blueB),
      inh = MSet()
    )

    val system = Set(trn).toSystem

    system.next(MSet(redA)).shouldBe(Set(MSet(blueB)))

  test("effect can depend on the color of the consumed token"):
    val redA = CToken("A", "red")

    val trn = ColoringTrn[String, String](
      cond = MSet(redA),
      eff = consumed =>
        if consumed.matches(MSet(CToken("A", "red"))) then
          MSet(CToken("B", "dark-red"))
        else
          MSet(CToken("B", "unknown")),
      inh = MSet()
    )

    val system = Set(trn).toSystem

    system.next(MSet(redA)).shouldBe(Set(MSet(CToken("B", "dark-red"))))

  test("other tokens stay in the marking after firing"):
    val redA = CToken("A", "red")
    val greenX = CToken("X", "green")

    val trn = ColoringTrn(
      cond = MSet(redA),
      eff = _ => MSet(CToken("B", "blue")),
      inh = MSet()
    )

    val system = Set(trn).toSystem

    system.next(MSet(redA, greenX)).shouldBe(
      Set(MSet(CToken("B", "blue"), greenX))
    )

  test("inhibitor prevents firing"):
    val redA = CToken("A", "red")
    val blackX = CToken("X", "black")

    val trn = ColoringTrn(
      cond = MSet(redA),
      eff = _ => MSet(CToken("B", "blue")),
      inh = MSet(blackX)
    )

    val system = Set(trn).toSystem

    system.next(MSet(redA, blackX)).shouldBe(Set.empty)
