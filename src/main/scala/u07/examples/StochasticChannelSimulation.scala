package u07.examples

import u07.utils.Time
import java.util.Random
import u07.examples.StochasticChannel.*

@main def mainStochasticChannelSimulation = {
  Time.timed:
    println:
      stocChannel.newSimulationTrace(IDLE, new Random)
        .take(10)
        .toList
        .mkString("\n")

  val rnd = new Random
  val timeUntilDone = stocChannel.statisticsUntil(IDLE,DONE, rnd)
  println("Time until done state:" + timeUntilDone.totalTime)
  println("Times in each state:" + timeUntilDone.stateTimes)

  val avgTimeUntilDone = stocChannel.averageCompletionTime(10, IDLE, DONE, rnd)
  val avgTimeInFailState = stocChannel.averageFractionInState(10, IDLE, DONE, FAIL, rnd);
  println("average time until done: " + avgTimeUntilDone)
  println("avg time in each state: " + avgTimeInFailState)
}
