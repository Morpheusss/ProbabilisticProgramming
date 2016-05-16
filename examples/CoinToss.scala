package examples

import definitions.{PNum, Experiment, Environment}
import distributions.discrete.Bernoulli


object CoinToss {

  def main(args: Array[String]) {

    val c1 : PNum = new Bernoulli(0.4)
    val c2 : PNum = new Bernoulli(0.7)
    val c3 : PNum = new Bernoulli(0.3)
    val exp : Experiment = new Experiment({
      environ : Environment => {
        if ((c1 getSampledValue environ) == 1) c2 getSampledValue environ
        else c3 getSampledValue environ
        environ
    }})

    var result : PNum = exp.reduceToMeasure((environ: Environment) => {
          if (c2 isSampledIn environ) c2 getSampledValue environ
          else c3 getSampledValue environ
        })

    println("========== Unconditioned Coin Tosses ==========")
    println("Mean for second toss: " + result.expectation(0.05, 0.05) + " (true value: 0.46)")
    println("Variance for second toss: " + result.variance(0.05, 0.05) + "  (true value: 0.2484)")
    println("Number of samples used: " + result.takenSamples.length)
    println("========== End Unconditioned Coin Tosses ==========")

    val conditioned : Experiment = exp.conditionedOn({env : Environment => c2 isSampledIn env})

    result = conditioned.reduceToMeasure((environ: Environment) => {
          if (c2 isSampledIn environ) c2 getSampledValue environ
          else c3 getSampledValue environ
        })  //  Same function, result of the second coin toss.

    println("========== Coin Tosses Conditioned on Coin 2 being Tossed ==========")
    println("Mean for second toss: " + result.expectation(0.05, 0.05) + " (true value: 0.7)")
    println("Variance for second toss: " + result.variance(0.05, 0.05) + " (true value: 0.21)")
    println("Number of samples used: " + result.takenSamples.length)
    println("========== End Coin Tosses Conditioned on Coin 2 being Tossed ==========")

  }

}
