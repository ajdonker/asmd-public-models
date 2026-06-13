package u06.modelling

package u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import _root_.u06.examples.ReadersWritersNet.*
import _root_.u06.utils.MSet.*
class ReadersWritersNetSpec extends AnyFunSuite:

  import _root_.u06.examples.ReadersWritersNet.*
  import _root_.u06.utils.MSet.*
  private val k = 10
  private val initialMarkings = MSet.ofList(List.fill(k)(P1) :+ P5)

  private val reachableMarkings =
    RW.paths(initialMarkings, 10).toSet.flatten

  test("PN should never have read and write active at the same time"):
    assert(reachableMarkings.forall(m => !(m(P6) > 0 && m(P7) > 0)))

  test("PN should never have more than one writer"):
    assert(reachableMarkings.forall(m => m(P7) <= 1))

  test("PN should preserve exactly one writer mutex in P5/P7"):
    assert(reachableMarkings.forall(m => m(P5) + m(P7) == 1))

  test("PN with no loopbacks should only have tokens in P5/P6/P7 at end"):
    val deadlocks =
      rwAcyclic
        .paths(init(5), 10)
        .map(_.last)
        .filter(m => rwAcyclic.paths(m, 2).forall(_.size == 1))
        .toSet

    val bad =
      deadlocks.filter(m =>
        m(P1) != 0 || m(P2) != 0 || m(P3) != 0 || m(P4) != 0
      )

    println("Deadlocks:")
    deadlocks.foreach(println)

    println("Bad deadlocks:")
    bad.foreach(println)

    assert(bad.isEmpty)

  test("every reachable waiting-reader marking enters reading later"):
    val initialMarkings = init(10)

    val reachable =
      eventualReadRW.paths(initialMarkings, 10).toSet.flatten

    val waitingReaderMarkings =
      reachable.filter(m => m(P3) > 0)

    assert(waitingReaderMarkings.nonEmpty)

    val witnessBound = 10

    assert(waitingReaderMarkings.forall { m =>
      eventualReadRW
        .paths(m, witnessBound)
        .exists(path =>
          path.exists(next => next(P3) < m(P3) && next(P6) > m(P6))
        )
    })
