package examples.evaluations

import distributions.continuous.Gaussian
import distributions.discrete.DiscreteValues

/**
  * Class: Testing class for generating an overview of estimation precision for expected values versus sample size
  */
object SampleSize {
  def main(args: Array[String]) {
    for (alpha <- Array(0.2, 0.1, 0.05, 0.01, 0.005)) {
      for (epsilon <- Array(1, 0.5, 0.2, 0.1, 0.05, 0.01)) {
        val num = new DiscreteValues(Map(1.0->0.2, 2.0->0.2, 3.0->0.2, 4.0->0.2, 5.0->0.2))
//        num.expectation(alpha, epsilon)
        num.variance(alpha, epsilon)
        println(s"$alpha, $epsilon, ${num.takenSamples.length}")
      }
    }
  }
}
