package examples

import definitions.{PNum, Experiment}
import distributions.continuous.Gaussian
import distributions.continuous.Arbitrary.fromPDF

/**
 * Intention(s): Test for MCMC continuous PNum construction
 */
object ArbitraryContinuous {

  def main(args: Array[String]) {

    val normalCurve : Double => Double = (x : Double) =>
      math.exp(-(x*x)/2) / math.sqrt(2 * math.Pi)


    val unnormalizedNormalCurve : Double => Double = (x : Double) =>
      math.exp(-(x*x)/2)

    //  Ideally, these 3 PNum's should follow the same distribution
    val default : PNum = new Gaussian(0, 1)
    val normalized : PNum = fromPDF(normalCurve)
    val unnormalized : PNum = fromPDF(unnormalizedNormalCurve)

    //  Partition of x-coordinate into :
    // (-inf, -3.5) (-3.5, -1.5) (-2.5, -1.5) (-1.5, -0.5) (-0.5, 0.5)
    //  (0.5, 1.5) (1.5, 2.5) (2.5, 3.5) (3.5, inf)
    val resultDefault : Array[Int] = Array.fill[Int](9)(0)
    val resultNormalized : Array[Int] = Array.fill[Int](9)(0)
    val resultUnnormalized : Array[Int] = Array.fill[Int](9)(0)

    //  Simulations
    for (i <- 1 to 3000) {
      val s = default getSampledValue new Experiment(env => {env.resolve(default); env} ).sample()
      if (s >= 3.5) resultDefault(8) += 1
      else if (s >= 2.5) resultDefault(7) += 1
      else if (s >= 1.5) resultDefault(6) += 1
      else if (s >= 0.5) resultDefault(5) += 1
      else if (s >= -0.5) resultDefault(4) += 1
      else if (s >= -1.5) resultDefault(3) += 1
      else if (s >= -2.5) resultDefault(2) += 1
      else if (s >= -3.5) resultDefault(1) += 1
      else resultDefault(0) += 1
    }

    for (i <- 1 to 3000) {
      val s = normalized getSampledValue new Experiment(env => {env.resolve(normalized); env} ).sample()
      if (s >= 3.5) resultNormalized(8) += 1
      else if (s >= 2.5) resultNormalized(7) += 1
      else if (s >= 1.5) resultNormalized(6) += 1
      else if (s >= 0.5) resultNormalized(5) += 1
      else if (s >= -0.5) resultNormalized(4) += 1
      else if (s >= -1.5) resultNormalized(3) += 1
      else if (s >= -2.5) resultNormalized(2) += 1
      else if (s >= -3.5) resultNormalized(1) += 1
      else resultNormalized(0) += 1
    }

    for (i <- 1 to 3000) {
      val s = unnormalized getSampledValue new Experiment(env => {env.resolve(unnormalized); env} ).sample()
      if (s >= 3.5) resultUnnormalized(8) += 1
      else if (s >= 2.5) resultUnnormalized(7) += 1
      else if (s >= 1.5) resultUnnormalized(6) += 1
      else if (s >= 0.5) resultUnnormalized(5) += 1
      else if (s >= -0.5) resultUnnormalized(4) += 1
      else if (s >= -1.5) resultUnnormalized(3) += 1
      else if (s >= -2.5) resultUnnormalized(2) += 1
      else if (s >= -3.5) resultUnnormalized(1) += 1
      else resultUnnormalized(0) += 1
    }

    //  Results
    println(resultDefault.mkString(", "))
    println(resultNormalized.mkString(", "))
    println(resultUnnormalized.mkString(", "))

  }

}
