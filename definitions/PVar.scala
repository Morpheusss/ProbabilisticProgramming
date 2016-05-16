package definitions

import distributions.distributions
import definitions.Global._

/**
 * Class: Base for any variable that is probabilistic
 */
class PVar [+T](val sample: (Environment) => T) {

  //  Assign the next available id
  val id = {
    distributions.nextId += 1
    distributions.nextId
  }

  def getSampledValue(environ : Environment = env) : T = environ.resolve(this)

  def isSampledIn(environ : Environment = env) : Boolean = environ.hasResolved(this)

}
