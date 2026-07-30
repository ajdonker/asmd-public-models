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

    }

    private def statisticsUntil(start: S, end: Event[S] => Boolean, finalTime: Event[S] => Double, rnd: Random): RunStatistics[S] =
      val trace =
        self.newSimulationTrace(start, rnd)

      val untilEnd =
        trace.takeWhile(event => !end(event)).toList

      val endEvent =
        trace.drop(untilEnd.size).head

      val events = untilEnd :+ endEvent

      val observationEnd = finalTime(endEvent)
      val stateTimes =
        events
          .sliding(2)
          .foldLeft(Map.empty[S, Double].withDefaultValue(0.0)):
            case (acc, Seq(e1, e2)) =>
              val intervalEnd = math.min(e2.time, observationEnd)

              val duration = math.max(0.0, intervalEnd - e1.time)
              acc.updated(
                e1.state,
                acc(e1.state) + duration
              )

            case (acc, _) => acc

      RunStatistics(
        observationEnd,
        stateTimes
      )

    def statisticsUntilEvent(start: S, isEnd: S => Boolean, rnd: Random): RunStatistics[S] =
      statisticsUntil(start, event => isEnd(event.state), event => event.time, rnd)

    def statisticsUntilTime(start: S, maxTime: Double, rnd: Random): RunStatistics[S] =
      statisticsUntil(start, event => event.time >= maxTime, _ => maxTime, rnd)

    def averageCompletionTime(    runs: Int,
                                  start: S,
                                  isStop: S => Boolean,
                                  rnd: Random
                                ): Double =
      (1 to runs)
        .map(_ => statisticsUntilEvent(start, isStop, rnd).totalTime)
        .sum / runs

    private def averageFraction(runs: Int, getStatistics: () => RunStatistics[S], isTarget: S => Boolean): Double =
      (1 to runs)
        .map { _ =>
          val statistics = getStatistics()
          val targetTime =
            statistics.stateTimes.collect:
              case (state, time) if isTarget(state) => time
            .sum

          targetTime / statistics.totalTime
        }
        .sum / runs
    def averageFractionInState(runs: Int, start: S, isStop: S => Boolean, isTarget: S => Boolean, rnd: Random): Double =
      averageFraction(runs, () => statisticsUntilEvent(start, isStop, rnd), isTarget)
      
    def averageFractionInState(runs: Int, start: S, maxTime: Double, isTarget: S => Boolean, rnd: Random): Double =
      averageFraction(runs, () => statisticsUntilTime(start, maxTime, rnd), isTarget)

  extension[S] (trace: Trace[S] )
    def states: LazyList[S] =
      trace.map(_.state)

    def firstStates(n: Int): List[S] =
      trace.take(n).map(_.state).toList

    def untilTime(maxTime: Double): List[Event[S]] =
      trace.takeWhile(_.time <= maxTime).toList  


