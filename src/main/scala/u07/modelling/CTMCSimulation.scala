package u07.modelling

import java.util.Random
import u07.utils.Stochastics

import scala.u07.modelling.RunStatistics

object CTMCSimulation:

  case class Event[A](time: Double, state: A)
  type Trace[A] = LazyList[Event[A]]

  export CTMC.*

  extension [S](self: CTMC[S])
    def newSimulationTrace(s0: S, rnd: Random): Trace[S] = {
      LazyList.iterate(Event(0.0, s0)):
        case Event(t, s) =>
          if self.transitions(s).isEmpty
          then
            Event(t, s)
          else
            val choices = self.transitions(s) map (t => (t.rate, t.state))
            val next = Stochastics.cumulative(choices.toList)
            val sumR = next.last._1
            val choice = Stochastics.draw(next)(using rnd)
            Event(t + Math.log(1 / rnd.nextDouble()) / sumR, choice)

      // can i add to trace methods below
    }

    def statisticsUntil(start: S, end: S, rnd: Random): RunStatistics[S] =
      val trace =
        self.newSimulationTrace(start, rnd)

      val untilEnd =
        trace.takeWhile(_.state != end).toList

      val endEvent =
        trace.drop(untilEnd.size).head

      val events = untilEnd :+ endEvent

      val stateTimes =
        events
          .sliding(2)
          .foldLeft(Map.empty[S, Double].withDefaultValue(0.0)):
            case (acc, Seq(e1, e2)) =>
              acc.updated(
                e1.state,
                acc(e1.state) + (e2.time - e1.time)
              )

      RunStatistics(
        events.last.time,
        stateTimes
      )

    def averageCompletionTime(    runs: Int,
                                  start: S,
                                  stop: S,
                                  rnd: Random
                                ): Double =
      (1 to runs)
        .map(_ => statisticsUntil(start, stop, rnd).totalTime)
        .sum / runs

    def averageFractionInState(
                                 runs: Int,
                                 start: S,
                                 stop: S,
                                 state: S,
                                 rnd: Random
                              ): Double =
      (1 to runs)
        .map { _ =>
          statisticsUntil(start, stop, rnd)
            .fractionIn(state)
        }
        .sum / runs


