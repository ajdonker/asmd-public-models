package u06.modelling

package u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import _root_.u06.examples.ReadersWritersNet.*
import _root_.u06.utils.MSet.*
case class SafetyProperty(description: String, property: MSet[Place] => Boolean):
  def check(markings: Set[MSet[Place]]): Boolean =
    markings.iterator.forall(property)

  def holds(marking: MSet[Place]): Boolean =
    property(marking)

  def checkPath(path: List[MSet[Place]]): Boolean =
    path.forall(property)

case class LivenessProperty(description: String, eventually: MSet[Place] => Boolean):
  def holdsInPaths(paths: Set[List[MSet[Place]]]): Boolean =
    paths.forall(path => path.exists(eventually))

case class BoundednessProperty(description: String, bound: (MSet[Place] => Int), limit: Int):
  def check(marking: MSet[Place]): Boolean = bound(marking) <= limit

// Safety properties
val mutualExclusion = SafetyProperty(
  "Mutual exclusion: no marking has both reader and writer active, max 1 writer",
  m => !(m(P6) > 0 && m(P7) > 0) && m(P7) <= 1
)

val mutexPreserved = SafetyProperty(
  "Mutex token preserved (P5 + P7 = 1)",
  m => m(P5) + m(P7) == 1
)

val eventualRead = LivenessProperty(
  "Every waiting reader eventually reads",
  m => m(P6) > 0
)

val boundedReaders = BoundednessProperty(
  "Readers bounded by 10",
  m => m(P6),
  10
)

class ReadersWritersNetSpec extends AnyFunSuite:
  import _root_.u06.examples.ReadersWritersNet.*
  import _root_.u06.utils.MSet.*
  private val k = 10
  private val initialMarkings = MSet.ofList(List.fill(k)(P1) :+ P5)

  private val reachableMarkings =
    RW.reachableMarkings(initialMarkings, 100)
  test("PN should never have read and write active at the same time"):
    assert(reachableMarkings.forall(m => !(m(P6) > 0 && m(P7) > 0)))

    assert(mutualExclusion.check(reachableMarkings))

  test("PN should never have more than one writer"):
    assert(reachableMarkings.forall(m => m(P7) <= 1))

  test("PN should preserve exactly one writer mutex in P5/P7"):
    assert(reachableMarkings.forall(m => m(P5) + m(P7) == 1))

  test("safety properties hold on all reachable states"):
    assert(mutualExclusion.check(reachableMarkings))
    assert(mutexPreserved.check(reachableMarkings))

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
