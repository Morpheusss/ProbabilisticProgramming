package definitions

import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.distribution.{TDistribution, ChiSquaredDistribution}
import scala.annotation.switch
import scala.collection.mutable.ArrayBuffer
import definitions.Global.env

/**
 * Class: Probabilistic number
 * Intention(s): wrapper for a numeric variable that follows some probability distribution
 * Parameter(s): takeSample - function that upon evaluation returns a random sample
 */
class PNum (sample: Environment => Double) extends PVar[Double](sample){

  //  Unary operators
  def unary_+ : PNum = this

  def unary_- : PNum = new PNum(env => -(this getSampledValue env))

  //  Arithmetic rules with constant values
  def ==(x: Short): PBool = new PBool(env => (this getSampledValue env) == x.toDouble)

  def ==(x: Int): PBool = new PBool(env => (this getSampledValue env) == x.toDouble)

  def ==(x: Long): PBool = new PBool(env => (this getSampledValue env) == x.toDouble)

  def ==(x: Float): PBool = new PBool(env => (this getSampledValue env) == x.toDouble)

  def ==(x: Double): PBool = new PBool(env => (this getSampledValue env) == x)

  def >(x: Short): PBool = new PBool(env => (this getSampledValue env) > x.toDouble)

  def >(x: Int): PBool = new PBool(env => (this getSampledValue env) > x.toDouble)

  def >(x: Long): PBool = new PBool(env => (this getSampledValue env) > x.toDouble)

  def >(x: Float): PBool = new PBool(env => (this getSampledValue env) > x.toDouble)

  def >(x: Double): PBool = new PBool(env => (this getSampledValue env) > x)

  def /(x: Short): PNum = new PNum(env => (this getSampledValue env) / x.toDouble)

  def /(x: Int): PNum = new PNum(env => (this getSampledValue env) / x.toDouble)

  def /(x: Long): PNum = new PNum(env => (this getSampledValue env) / x.toDouble)

  def /(x: Float): PNum = new PNum(env => (this getSampledValue env) / x.toDouble)

  def /(x: Double): PNum = new PNum(env => (this getSampledValue env) / x)

  def !=(x: Short): PBool = new PBool(env => (this getSampledValue env) != x.toDouble)

  def !=(x: Int): PBool = new PBool(env => (this getSampledValue env) != x.toDouble)

  def !=(x: Long): PBool = new PBool(env => (this getSampledValue env) != x.toDouble)

  def !=(x: Float): PBool = new PBool(env => (this getSampledValue env) != x.toDouble)

  def !=(x: Double): PBool = new PBool(env => (this getSampledValue env) != x)

  def +(x: Short): PNum = new PNum(env => (this getSampledValue env) + x.toDouble)

  def +(x: Int): PNum = new PNum(env => (this getSampledValue env) + x.toDouble)

  def +(x: Long): PNum = new PNum(env => (this getSampledValue env) + x.toDouble)

  def +(x: Float): PNum = new PNum(env => (this getSampledValue env) + x.toDouble)

  def +(x: Double): PNum = new PNum(env => (this getSampledValue env) + x)

  def %(x: Short): PNum = new PNum(env => (this getSampledValue env) % x.toDouble)

  def %(x: Int): PNum = new PNum(env => (this getSampledValue env) % x.toDouble)

  def %(x: Long): PNum = new PNum(env => (this getSampledValue env) % x.toDouble)

  def %(x: Float): PNum = new PNum(env => (this getSampledValue env) % x.toDouble)

  def %(x: Double): PNum = new PNum(env => (this getSampledValue env) % x.toDouble)

  def <=(x: Short): PBool = new PBool(env => (this getSampledValue env) <= x.toDouble)

  def <=(x: Int): PBool = new PBool(env => (this getSampledValue env) <= x.toDouble)

  def <=(x: Long): PBool = new PBool(env => (this getSampledValue env) <= x.toDouble)

  def <=(x: Float): PBool = new PBool(env => (this getSampledValue env) <= x.toDouble)

  def <=(x: Double): PBool = new PBool(env => (this getSampledValue env) <= x)

  def <(x: Short): PBool = new PBool(env => (this getSampledValue env) < x.toDouble)

  def <(x: Int): PBool = new PBool(env => (this getSampledValue env) < x.toDouble)

  def <(x: Long): PBool = new PBool(env => (this getSampledValue env) < x.toDouble)

  def <(x: Float): PBool = new PBool(env => (this getSampledValue env) < x.toDouble)

  def <(x: Double): PBool = new PBool(env => (this getSampledValue env) < x)

  def -(x: Short): PNum = new PNum(env => (this getSampledValue env) - x.toDouble)

  def -(x: Int): PNum = new PNum(env => (this getSampledValue env) - x.toDouble)

  def -(x: Long): PNum = new PNum(env => (this getSampledValue env) - x.toDouble)

  def -(x: Float): PNum = new PNum(env => (this getSampledValue env) - x.toDouble)

  def -(x: Double): PNum = new PNum(env => (this getSampledValue env) - x)

  def >=(x: Short): PBool = new PBool(env => (this getSampledValue env) >= x.toDouble)

  def >=(x: Int): PBool = new PBool(env => (this getSampledValue env) >= x.toDouble)

  def >=(x: Long): PBool = new PBool(env => (this getSampledValue env) >= x.toDouble)

  def >=(x: Float): PBool = new PBool(env => (this getSampledValue env) >= x.toDouble)

  def >=(x: Double): PBool = new PBool(env => (this getSampledValue env) >= x)

  def *(x: Short): PNum = new PNum(env => (this getSampledValue env) * x.toDouble)

  def *(x: Int): PNum = new PNum(env => (this getSampledValue env) * x.toDouble)

  def *(x: Long): PNum = new PNum(env => (this getSampledValue env) * x.toDouble)

  def *(x: Float): PNum = new PNum(env => (this getSampledValue env) * x.toDouble)

  def *(x: Double): PNum = new PNum(env => (this getSampledValue env) * x)

  //  Arithmetic rules with PNum's
  def ==(x: PNum): PBool = new PBool(env => (this getSampledValue env) == (x getSampledValue env))

  def >(x: PNum): PBool = new PBool(env => (this getSampledValue env) > (x getSampledValue env))

  def /(x: PNum): PNum = new PNum(env => (this getSampledValue env) / (x getSampledValue env))

  def !=(x: PNum): PBool = new PBool(env => (this getSampledValue env) != (x getSampledValue env))

  def +(x: PNum): PNum = new PNum(env => (this getSampledValue env) + (x getSampledValue env))

  def %(x: PNum): PNum = new PNum(env => (this getSampledValue env) % (x getSampledValue env))

  def <=(x: PNum): PBool = new PBool(env => (this getSampledValue env) <= (x getSampledValue env))

  def <(x: PNum): PBool = new PBool(env => (this getSampledValue env) < (x getSampledValue env))

  def -(x: PNum): PNum = new PNum(env => (this getSampledValue env) - (x getSampledValue env))

  def >=(x: PNum): PBool = new PBool(env => (this getSampledValue env) >= (x getSampledValue env))

  def *(x: PNum): PNum = new PNum(env => (this getSampledValue env) * (x getSampledValue env))

  lazy val takenSamples : ArrayBuffer[Double] = ArrayBuffer.empty[Double]

  /*
  * This function estimates the expected value of this PNum,
  *   by repeatedly sampling and estimating the expected value
  *   until the confidence interval defined by the confidence level, alpha,
  *   spans a range shorter than errRange.
  * */
  def expectation(alpha : Double, errRange : Double, environ : Environment = env) : Double =
    if (alpha >= 0.5 || alpha <= 0) throw new IllegalArgumentException("alpha should be in range (0, 0.5)")
    else if (errRange <= 0) throw new IllegalArgumentException("tolerance should be in range (0, +infinity)")
    else {
      if (takenSamples.length < 64 ) takenSamples ++= 1 to (64 - takenSamples.length) map { _ => sample(environ)}
      var sum : Double = takenSamples.sum
      var tDist : TDistribution = new TDistribution(takenSamples.length-1)
      while (math.sqrt(StatUtils.variance(takenSamples.toArray, sum/takenSamples.length) / takenSamples.length) * tDist.inverseCumulativeProbability(1.0 - alpha/2) * 2 >= errRange) {
        val s = 1 to 16 map {_ => sample(environ)}
        takenSamples ++= s
        sum += s.sum
        tDist = new TDistribution(takenSamples.length-1)
      }
      sum / takenSamples.length
    }

  /*
  * This function estimates the variance of this PNum,
  *   by repeatedly sampling and estimating the variance
  *   until the confidence interval defined by the confidence level, alpha,
  *   spans a range shorter than errRange.
  * */
  def variance(alpha : Double, errRange : Double, environ : Environment = env) : Double =
    if (alpha >= 0.5 || alpha <= 0) throw new IllegalArgumentException("alpha should be in range (0, 0.5)")
    else if (errRange <= 0) throw new IllegalArgumentException("tolerance should be in range (0, +infinity)")
    else {
      if (takenSamples.length < 64 ) takenSamples ++= 1 to (64 - takenSamples.length) map { _ => sample(environ)}
      var sum : Double = takenSamples.sum
      var chiSqDist : ChiSquaredDistribution = new ChiSquaredDistribution(takenSamples.length-1)
      var est : Double = StatUtils.variance(takenSamples.toArray, sum/takenSamples.length)
      while (((takenSamples.length-1) * est / chiSqDist.inverseCumulativeProbability(1.0 - alpha/2))
             - ((takenSamples.length-1) * est / chiSqDist.inverseCumulativeProbability(alpha/2))
             >= errRange) {
        val s = 1 to 16 map {_ => sample(environ)}
        takenSamples ++= s
        sum += s.sum
        chiSqDist = new ChiSquaredDistribution(takenSamples.length-1)
        est = StatUtils.variance(takenSamples.toArray, sum/takenSamples.length)
      }
      est
    }

    /*
    * This function estimates the PMF of the random variable
    *  represented by this PNum, assuming that it is discrete.
    * Algorithm implemented: kernel density estimation
    * */
    def estimatedPMF (minSamples : Int = 128, environ : Environment = env) : Double => Double = {
      if (takenSamples.length < minSamples) takenSamples ++= 1 to (minSamples - takenSamples.length) map {_ => sample(environ)}
      x => takenSamples.fold[Double](0)(
        (acc, cur) => if (cur == x) acc+1
                      else acc
      ) / takenSamples.length
    }

    /*
    * This function estimates the PDF of the random variable
    *  represented by this PNum, assuming that it is continuous.
    * Algorithm implemented: kernel density estimation
    * */
    def estimatedPDF (bandwidth : Double, kernel : String = "epanechnikov", minSamples : Int = 128, environ : Environment = env) : Double => Double = {
      if (takenSamples.length < minSamples) takenSamples ++= 1 to (minSamples - takenSamples.length) map {_ => sample(environ)}
      val K : Double => Double = (kernel : @switch) match {
        case "uniform" =>
          x => if (-1 <= x && x <= 1) 0.5 else 0
        case "triangular" =>
          x => if (-1 <= x && x <= 1) 1 - math.abs(x) else 0
        case "gaussian" =>
          x => math.exp(-(x*x)/2) / math.sqrt(2 * math.Pi)
        case "epanechnikov" =>
          x => if (-1 <= x && x <= 1) 0.75*(1 - x*x) else 0
        case _ =>
          throw new IllegalArgumentException("Cannot understand the given kernel name")
      }
      (x : Double) => takenSamples.fold[Double](0)(
        (acc, cur) => acc + K((x - cur) / bandwidth)
      ) / (takenSamples.length * bandwidth)
    }

}
