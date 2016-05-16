package distributions.discrete

import definitions.Global._
import definitions.{Environment, PNum}
import distributions.distributions.{random0to1}

/**
 * Class: Bernoulli
 * Intention(s): Encapsulates Bernoulli distribution
 * Parameter(s): p - probability of success
 */
class Bernoulli (p : Double) extends PNum(_ => if (random0to1() < p) 1 else 0){
  override def expectation(alpha : Double, tolerance : Double, environ : Environment = env) : Double = p
  override def variance(alpha : Double, tolerance : Double, environ : Environment = env) : Double = p * (1.0-p)
}
