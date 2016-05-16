package examples

import definitions.PNum
import distributions.discrete.Bernoulli

/**
  * Test for simple creation of PVars
  */
object PVarCreation {
  def main(args: Array[String]) {
    val x : PNum = new Bernoulli(0.5)
    1 to 20 foreach {i => println(x.expectation(0.005, 1.0 / i))}
    1 to 5 foreach {_ => println(x == 1 getSampledValue())}
  }
}
