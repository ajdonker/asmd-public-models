package u07.examples

import java.util.Random

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import u07.utils.{Grids, MSet}
import u07.modelling.DAP.*
import u07.modelling.CTMCSimulation.*
import StochasticReadersWriters.*
import StochasticReadersWriters.Place.*

class StochasticReadersWritersSpec extends AnyFunSuite:

  private val k = 6

  private val initial : MSet[Place] = initMarking

  private def markingOf(s: State[ID, Place]): MSet[Place] =
    MSet.ofList(
      s.tokens.asList.map:
        case Token(_, place) => place
    )

  private def simulate(seed: Long, steps: Int): List[MSet[Place]] =
      readersWritersCTMC
      .newSimulationTrace(initial, new Random(seed))
      .take(steps)
      .map(_.state)
      .toList


  test("initial marking has k processes and one mutex"):
    initial(START).shouldBe(k)
    initial(MUTEX).shouldBe(1)

    initial(ARRIVAL).shouldBe(0)
    initial(READ_WAIT).shouldBe(0)
    initial(READ).shouldBe(0)
    initial(WRITE_WAIT).shouldBe(0)
    initial(WRITE).shouldBe(0)

  test("SPN should preserve the number of processes"):
    val seeds = 1L to 100L

    for seed <- seeds do
      val reachableMarkings =
        simulate(seed, 500)

      assert(
        reachableMarkings.forall: marking =>
          val processTokens =
            marking(START) +
              marking(ARRIVAL) +
              marking(READ_WAIT) +
              marking(READ) +
              marking(WRITE_WAIT) +
              marking(WRITE)

          processTokens == k,
        s"Process-token conservation failed for seed $seed"
      )

  test("SPN should never have readers and a writer active together"):
    val seeds = 1L to 100L

    for seed <- seeds do
      val reachableMarkings =
        simulate(seed, 500)

      assert(
        reachableMarkings.forall: marking =>
          !(marking(READ) > 0 && marking(WRITE) > 0),
        s"Read/write mutual exclusion failed for seed $seed"
      )

  test("SPN should never have more than one writer"):
    val seeds = 1L to 100L

    for seed <- seeds do
      val reachableMarkings =
        simulate(seed, 500)

      assert(
        reachableMarkings.forall: marking =>
          marking(WRITE) <= 1,
        s"Multiple writers found for seed $seed"
      )

  test("SPN should preserve exactly one writer mutex"):
    val seeds = 1L to 100L

    for seed <- seeds do
      val reachableMarkings =
        simulate(seed, 500)

      assert(
        reachableMarkings.forall: marking =>
          marking(MUTEX) + marking(WRITE) == 1,
        s"Writer mutex invariant failed for seed $seed"
      )
