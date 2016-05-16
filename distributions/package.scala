package distributions

import scala.util.Random

/*
* Package object - containing helper functions for internal uses only.
* */
package object distributions {

  private val rng = new Random()

  //  Random number genrating methods
  def randomInt(max : Int) : Int = rng.nextInt(max)
  def random0to1() : Double = rng.nextDouble()
  def randomBoolean() : Boolean = rng.nextBoolean()
  def randomGaussian() : Double = rng.nextGaussian()

  var nextId : Long = 0

}
