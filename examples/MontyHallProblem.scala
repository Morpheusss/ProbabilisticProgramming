package examples

import definitions.{PVar, PBool, PNum, Experiment}
import distributions.discrete.DiscreteValues
import definitions.Global._

/*
* Simulate the famous Monty Hall Problem,
*  and test the winning rate of two different strategies.
* It is safe to assume that the car is behind door 1.
* */
object MontyHallProblem {
  def main(args: Array[String]) {
    //  Generalize the original problem to arbitrary number of doors.
    //  Switching of option results in any arbitrary door that is left unopened.
    val numDoors = 3
    var playersChoice : PNum = null
    val winEvent : PBool = new Experiment (env => {
      println("Player's reasoning time!!")
      //  Initializing variable and their relations
      var initialOutcomes = Map[Double, Double]()
      1 to numDoors foreach {initialOutcomes += _.toDouble -> 1.0/numDoors}
      val initialChoice : PNum = new DiscreteValues(initialOutcomes)
      val choicesForHost : PVar[Set[Int]] = new PVar[Set[Int]](env => {
        var s = Set[Int]()
        s ++= 2 to numDoors
        s - (initialChoice getSampledValue env).toInt
      })
      val hostChoice : PNum = new PNum (env => {
        val choices : Set[Int] = choicesForHost getSampledValue env
        var mappings : Map[Double, Double] = Map()
        choices foreach {mappings += _.toDouble -> 1.0/choices.size}
        new DiscreteValues(mappings).sample(env)
      })
      val switchChoice : PNum = new PNum (env => {
        var choices : Set[Int] = (choicesForHost getSampledValue env) - (hostChoice getSampledValue env).toInt
        if ((initialChoice getSampledValue env) != 1) choices += 1
        var mappings : Map[Double, Double] = Map()
        choices foreach {mappings += _.toDouble -> 1.0/choices.size}
        new DiscreteValues(mappings).sample(env)
      })

      //  Simulating and playing the game
      println("Simulating no-switching scenario!!")
      val insist : PBool = new Experiment(env => {
        initialChoice getSampledValue env
        env
      }).reduceToEvent(env => (initialChoice getSampledValue env) == 1)
      val estimatedInsist = insist.probability(0.05, 0.05)
      println(s"Winning rate if insist option: $estimatedInsist!!")

      println("Simulating switching scenario!!")
      val switch : PBool = new Experiment(env => {
        switchChoice getSampledValue env
        env
      }).reduceToEvent(env => (switchChoice getSampledValue env) == 1)
      val estimatedSwitch = switch.probability(0.05, 0.05)
      println(s"Winning rate if switch option: $estimatedSwitch!!")

      if (estimatedInsist >= estimatedSwitch) {
        println("Player's chosen the insisting strategy!")
        playersChoice = initialChoice
      }
      else {
        println("Player's chosen the switching strategy!")
        playersChoice = switchChoice
      }
      playersChoice getSampledValue env
      env
      }).reduceToEvent(env => (playersChoice getSampledValue env) == 1)

    println(if (winEvent.sample(env)) "Player got the car!!!" else "Player missed the car!!!")
  }
}
