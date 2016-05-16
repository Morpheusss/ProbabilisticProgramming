package examples

import definitions.{PNum, Experiment}
import distributions.discrete.Bernoulli

/**
 * Intention: Test for basic dependency issues
 */
object Basics {

  def main(args: Array[String]) {

    val x1 : PNum = new Bernoulli(0.5)
    val x2 : PNum = new Bernoulli(0.5)
    val x3 : PNum = new Bernoulli(0.5)
    val x4 : PNum = new Bernoulli(0.5)

    val x12 : PNum = x1 + x2
    val x22 : PNum = x2 + x2
    val x23 : PNum = x2 + x3
    val x34 : PNum = x3 + x4

    val x1223 : PNum = x12 + x23
    val x1234 : PNum = x12 + x34
    val x2222 : PNum = x22 + x22

    val result1223 : Array[Int] = Array.fill[Int](5)(0)
    val result1234 : Array[Int] = Array.fill[Int](5)(0)
    val result2222 : Array[Int] = Array.fill[Int](5)(0)

    for (i <- 1 to 1000) {
      val experiment = new Experiment(env => {
        x1223 getSampledValue env
        env
      })
      result1223( x1223.getSampledValue(experiment.sample()).toInt ) += 1
    }

    for (i <- 1 to 1000) {
      val experiment = new Experiment(env => {
        x1234 getSampledValue env
        env
      })
      result1234( x1234.getSampledValue(experiment.sample()).toInt ) += 1
    }

    for (i <- 1 to 1000) {
      val experiment = new Experiment(env => {
        x2222 getSampledValue env
        env
      })
      result2222( x2222.getSampledValue(experiment.sample()).toInt ) += 1
    }

    println(result1223.mkString(", "))  //  "Flattened" binomial
    println(result1234.mkString(", "))  //  Standard binomial
    println(result2222.mkString(", "))  //  Bernoulli on (1,5) with p = 0.5

  }

}
