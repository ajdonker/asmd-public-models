package u07.examples

import u07.utils.*
import java.util.Random
import u07.modelling.{CTMC, CTMCSimulation, SPN}
import u07.modelling.SPN.Trn
import u07.examples.StochasticChannel.*
import u07.modelling.CTMCSimulation.*

@main def mainStochasticChannelSimulation = {
  Time.timed:
    println:
      stocChannel.newSimulationTrace(IDLE, new Random)
        .take(10)
        .toList
        .mkString("\n")

  val rnd = new Random
  val timeUntilDone = stocChannel.statisticsUntilEvent(IDLE,state => state == DONE, rnd)
  println("Time until done state:" + timeUntilDone.totalTime)
  println("Times in each state:" + timeUntilDone.stateTimes)

  val avgTimeUntilDone = stocChannel.averageCompletionTime(10, IDLE, state => state == DONE, rnd)
  println("Average time until done" + avgTimeUntilDone)
}
