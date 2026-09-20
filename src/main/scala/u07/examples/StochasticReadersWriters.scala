package u07.examples
import java.util.Random
import u07.modelling.{CTMC, CTMCSimulation, SPN}
import u07.modelling.CTMCSimulation.*
import u07.modelling.SPN.Trn
import u07.utils.{Grids, MSet, Time}
object StochasticReadersWriters:
  enum Place:
    case ARRIVAL, START, READ_WAIT, READ, WRITE_WAIT, WRITE, MUTEX
  type ID = (Int, Int)

  export Place.*
  export u07.modelling.SPN.*
  export u07.modelling.CTMCSimulation.*

// use SPN not DAP

  val srw = SPN[Place](
    Trn(MSet(START), m => 1.0, MSet(ARRIVAL), MSet()),
    Trn(MSet(ARRIVAL), m => 200000, MSet(READ_WAIT), MSet()),
    Trn(MSet(ARRIVAL), m => 100000, MSet(WRITE_WAIT), MSet()),
    Trn(MSet(READ_WAIT, MUTEX), m => 100000, MSet(MUTEX, READ), MSet()),
    Trn(MSet(WRITE_WAIT, MUTEX), m => 100000, MSet(WRITE), MSet(READ)),
    Trn(MSet(READ), m => 0.1*m(READ), MSet(START), MSet()),
    Trn(MSet(WRITE), m => 0.2, MSet(START, MUTEX), MSet())
  )
  val readersWritersCTMC = toCTMC(srw)
  val initMarking: MSet[Place] = MSet(START, START, START, START, START, START, MUTEX)
  @main
  def mainStochasticReadersWriters = {
    val rnd = new Random()
    Time.timed:
      println:
        readersWritersCTMC
          .newSimulationTrace(initMarking, rnd)
          .take(10)
          .toList
          .mkString("\n")

    val timeUntilWrite = readersWritersCTMC.statisticsUntilEvent(
      initMarking,
      marking => marking(WRITE) > 0,
      rnd
    )
    println("Time until write: " + timeUntilWrite.totalTime)

    val avgTimeReadingUntilWrite = readersWritersCTMC.statisticsUntilEvent(
      initMarking, marking => marking(WRITE) > 0,
      rnd
    )
    val timeReading = avgTimeReadingUntilWrite.stateTimes.collect {   case (marking, time) if marking(READ) > 0 => time }.sum 
    println("Time spent reading until a write: " + timeReading)
  }

