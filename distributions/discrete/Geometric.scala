package distributions.discrete

import definitions.Global._
import definitions.{Environment, PNum}
import distributions.distributions.random0to1

/**
 * Class: Geometric
 * Intention(s): Encapsulates geometric distribution.
 * Parameter(s): p - success rate
 */
class Geometric (p : Double) extends PNum(_ => {
  var count : Double = 1
  while (random0to1() > p) {
    count += 1
  }
  count
}) {
  override def expectation(alpha : Double, tolerance : Double, environ : Environment = env) : Double = 1.0/p
  override def variance(alpha : Double, tolerance : Double, environ : Environment = env) : Double = (1.0-p) / (p*p)
}
