package definitions

import definitions.Global._

/**
 * Class: Simulation
 * Intention(s): Simulation-level functions
 */
class Experiment (sim: Environment => Environment){

  /*
  * Return a sample from the unconditioned distribution,
  *  given the current state/environment.
  * */
  def sample(iniEnv : Environment = env) : Environment = {
    val envOriginal = env
    env = iniEnv.extendNew
    val ret = sim(env)
    env = envOriginal
    ret
  }

  /*
  * Return a new Experiment conditioned on the condition
  * defined by the argument.
  * Argument: cond - a function that takes a environment and
  *  returns whether some condition is satisfied.
  * Algorithm implemented: Rejection sampling.
  * A count of fails is kept to avoid looping to satisfy unsatisfiable conditions.
  * Default value of maxTrial is defined as the number of trials needed
  *  for a condition satisfied with probability 0.01 to generate
  *  a all-unsatisfied sequence of samples with probability at most 0.01
  *  (in other words, if the condition given can be satisfied with probability at least 0.01,
  *   then, it should be satisfied within 459 trials with probability 0.99)
  * */
  def conditionedOn( cond : Environment => Boolean, maxTrials : Int = 459) : Experiment =
    new Experiment({ iniEnv : Environment => {
      var s = sample(iniEnv)
      var numFails = 0
      while (numFails < maxTrials && !cond(s)) {
        s = sample(iniEnv)
        numFails += 1
      }
      if (cond(s)) s
      else throw new IllegalArgumentException("Given condition cannot be satisfied.")
    }})

  /*
  * Return a PNum that represents the interested numerical measure
  *  as a separate random variable.
  * */
  def reduceToMeasure(func : Environment => Double, environ : Environment = env ) : PNum =
    new PNum(_ => {
      val envOriginal = env
      env = sample(environ)
      val ret = func(env)
      env = envOriginal
      ret
    })

  /*
  * Return a PBool that represents whether an event happened or not.
  * */
  def reduceToEvent(func : Environment => Boolean, environ : Environment = env ) : PBool =
    new PBool( _ => {
      val envOriginal = env
      env = sample(environ)
      val ret = func(env)
      env = envOriginal
      ret
    })

}
