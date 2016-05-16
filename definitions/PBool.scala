package definitions

import definitions.Global._
import org.apache.commons.math3.distribution.TDistribution
import org.apache.commons.math3.stat.StatUtils

import scala.collection.mutable.ArrayBuffer

/**
 * Class: Probability booleans - boolean variables following some distribution
 * Intention(s): Provide easy way to express boolean-valued functions over PNum
 * Parameter(s): takeSample - function that returns a sample
 */
class PBool (sample : Environment => Boolean) extends PVar[Boolean](sample) {

  def unary_! : PBool = new PBool(environ => !(this getSampledValue environ))

  //  Arithmetic rules with Boolean
  def ||(x: Boolean): PBool = new PBool(environ => (this getSampledValue environ) || x)

  def ==(x: Boolean): PBool = new PBool(environ => (this getSampledValue environ) == x)

  def &(x: Boolean): PBool = new PBool(environ => (this getSampledValue environ) & x)

  def &&(x: Boolean): PBool = new PBool(environ => (this getSampledValue environ) && x)

  def `|`(x: Boolean): PBool = new PBool(environ => (this getSampledValue environ) | x)

  def !=(x: Boolean): PBool = new PBool(environ => (this getSampledValue environ) != x)

  def ^(x: Boolean): PBool = new PBool(environ => (this getSampledValue environ) ^ x)


  //  Arithmetic rules with PBool
  def ||(x: PBool): PBool = new PBool(environ => (this getSampledValue environ) || (x getSampledValue environ))

  def ==(x: PBool): PBool = new PBool(environ => (this getSampledValue environ) == (x getSampledValue environ))

  def &(x: PBool): PBool = new PBool(environ => (this getSampledValue environ) & (x getSampledValue environ))

  def &&(x: PBool): PBool = new PBool(environ => (this getSampledValue environ) && (x getSampledValue environ))

  def `|`(x: PBool): PBool = new PBool(environ => (this getSampledValue environ) | (x getSampledValue environ))

  def !=(x: PBool): PBool = new PBool(environ => (this getSampledValue environ) != (x getSampledValue environ))

  def ^(x: PBool): PBool = new PBool(environ => (this getSampledValue environ) ^ (x getSampledValue environ))

  lazy val takenSamples :ArrayBuffer[Boolean] = ArrayBuffer.empty[Boolean]

  /*
  * This function estimates the probability of event represented by this PBool
  *   by treating the samples (T/F) as numerical values (1/0), and estimate
  *   the expected value of this numerical value.
  * Same approach is applied as estimating expected values:
  *   repeatedly sample and estimate the expected value
  *   until the confidence interval defined by the confidence level, alpha,
  *   spans a range shorter than errRange.
  * */
  def probability(alpha : Double, errRange : Double, environ : Environment = env) : Double =
    if (alpha >= 0.5 || alpha <= 0) throw new IllegalArgumentException("alpha should be in range (0, 0.5)")
    else if (errRange <= 0) throw new IllegalArgumentException("tolerance should be in range (0, +infinity)")
    else {
      if (takenSamples.length < 64 ) takenSamples ++= 1 to (64 - takenSamples.length) map { _ => sample(env)}
      val numSamples : ArrayBuffer[Double] = takenSamples.map(s => if (s) 1.0 else 0.0)
      var sum : Double = 0
      numSamples.foreach( sum += _ )
      var tDist : TDistribution = new TDistribution(numSamples.length-1)
      while (math.sqrt(StatUtils.variance(numSamples.toArray, sum/numSamples.length) / numSamples.length) * tDist.inverseCumulativeProbability(1.0 - alpha/2) * 2 >= errRange) {
        val s = 1 to 16 map {_ => sample(env)}
        takenSamples ++= s
        numSamples ++= s map (if (_) {sum += 1.0; 1.0} else 0.0)
        tDist = new TDistribution(numSamples.length-1)
      }
      sum / takenSamples.length
    }


}
