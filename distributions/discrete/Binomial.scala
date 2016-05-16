package distributions.discrete

import definitions.Global._
import definitions.{Environment, PNum}
import distributions.distributions.random0to1

/**
 * Class: Binomial
 * Intention(s): Encapsulates a binomial distribution.
 * Parameter(s): p - success rate
 *               n - number of trials
 */
class Binomial (p : Double, n : Int) extends PNum(_ => {
  var count : Double = 0
  for (i <- 1 to n) {
    count += (if (random0to1 < p) 1.0 else 0.0)
  }
  count
}) {
  override def expectation(alpha : Double, tolerance : Double, environ : Environment = env) : Double = n*p
  override def variance(alpha : Double, tolerance : Double, environ : Environment = env) : Double = n*p*(1.0-p)
}
