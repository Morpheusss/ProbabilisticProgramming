package examples

import org.apache.commons.math3.distribution.TDistribution

/*
* Testing class for computing critical values
* */
object ComputingCriticalValues {
  def main(args: Array[String]) {
    val dist = new TDistribution(10)
    for (i <- 1 to 9) {
      println(dist.inverseCumulativeProbability(i/10.0))
    }
  }
}
