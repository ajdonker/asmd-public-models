package scala.u07.modelling

case class RunStatistics[S] (totalTime: Double, stateTimes: Map[S, Double]):
  def timeIn(state: S): Double =
    stateTimes.getOrElse(state, 0.0)

  def fractionIn(state: S): Double =
    timeIn(state) / totalTime
    
