package u08.modelling

import java.util.Random

object CTMCExperiment:

  import CTMCSimulation.*

  opaque type Property[A] = Trace[A] => Boolean

  given rnd: Random = new Random

  extension [S](self: CTMC[S])
    // globally is simply achieved by equivalence not G x= F not x
    def eventually[A](filt: A => Boolean): Property[A] =
      trace => trace exists (e => filt(e.state))

    // takes a property and makes it time bounded by the magics of streams
    def bounded[A](timeBound: Double)(prop: Property[A]): Property[A] =
      trace => prop(trace takeWhile (_.time <= timeBound))

    def globally[A](filt: A => Boolean): Property[A] =
      trace => trace.forall(e => filt(e.state))

    def until[A](pred: A => Boolean, q: A => Boolean): Property[A] =
      trace =>
        val prefix = trace.takeWhile(e => !q(e.state))
        prefix.forall(e => pred(e.state)) &&
          trace.dropWhile(e => !q(e.state)).headOption.exists(e => q(e.state))

    // a PRISM-like experiment, giving a statistical result (in [0,1])
    def experiment(runs: Int, prop: Property[S], s0: S, timeBound: Double): Double =
      (0 until runs).count: _ =>
        bounded(timeBound)(prop)(self.newSimulationTrace(s0 ,rnd))
      .toDouble/runs

    // experiment G φ within time bound
    def experimentGlobally(runs: Int, filt: S => Boolean, s0: S, timeBound: Double): Double =
      experiment(runs, globally(filt), s0, timeBound)
    // experiment F φ within time bound
    def experimentEventually(runs: Int, filt: S => Boolean, s0: S, timeBound: Double): Double =
      experiment(runs, eventually(filt), s0, timeBound)

      // Long-run fraction of time spent in states satisfying filt in one run
    def steadyStateEstimateSingle(horizon: Double, filt: S => Boolean, s0: S): Double =
      val trace = self.newSimulationTrace(s0, rnd).takeWhile(_.time <= horizon).toList
      trace match
        case Nil => 0.0
        case _ =>
          val intervals =
            trace.zip(trace.drop(1)).map { case (curr, next) =>
              val dt = next.time - curr.time
              if filt(curr.state) then dt else 0.0
            }

          val lastTime = trace.last.time
          val totalTime = math.max(lastTime, 1e-9)
          intervals.sum / totalTime

      // Average long-run fraction over many runs
    def steadyStateEstimate(runs: Int, horizon: Double, filt: S => Boolean, s0: S): Double =
      (0 until runs).map { _ =>
        steadyStateEstimateSingle(horizon, filt, s0)
      }.sum / runs

    // Compare two predicates in steady state
    def compareSteadyState(
                            runs: Int,
                            horizon: Double,
                            f1: S => Boolean,
                            f2: S => Boolean,
                            s0: S
                          ): (Double, Double) =
      (
        steadyStateEstimate(runs, horizon, f1, s0),
        steadyStateEstimate(runs, horizon, f2, s0)
      )


