package distributions.continuous

import definitions.Global._
import definitions.{Environment, PNum}
import distributions.distributions.randomGaussian

/**
 * Class: Gaussian
 * Intention(s): Encapsulates a Gaussian/normal distribution.
 * Parameter(s): mean - distribution mean
 *               std  - standard deviation
 */
class Gaussian (mean : Double, std : Double) extends PNum(_ => randomGaussian() * std + mean) {
  override def expectation(alpha : Double, tolerance : Double, environ : Environment = env) : Double = mean
  override def variance(alpha : Double, tolerance : Double, environ : Environment = env) : Double = std * std
}
