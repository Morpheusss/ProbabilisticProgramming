package distributions.continuous

import definitions.{Environment, PNum}
import distributions.distributions._
import util.BoolRingBuffer

/**
 * Class: Builder of PNum from arbitrary distribution.
 * Parameter(s): pdf - probability density function (required!)
 *               initialGuess - MCMC initial guess
 *               initialSTD - MCMC jump function initial STD
 *               windowSize - acceptance history window size to tune sigma (STD)
 *               burnIn - number of initial records to discard
 *               sigmaAdjust - Adjusting rate for sigma (STD)
 */
object Arbitrary {

  //  Markov Chain Monte Carlo to create a sampler for PNum
  //  Proposal : random point with displacement conforming to a normal distribution
  //  Symmetric jump distribution <=> acc = pdf(x_p) / pdf(x)
  def fromPDF (pdf : Double => Double, initialGuess : Double = 0, initialSTD : Double = 1,
               windowSize : Int = 50, burnIn : Int = 1000, sigmaAdjust : Double = 1.2, stripe : Int = 10)
  : PNum =
  {
    var x = initialGuess
    var sigma = initialSTD
    val halfWindow = windowSize / 2
    val rb = new BoolRingBuffer(windowSize)
    val propose : (Double, Double) => Double = (x_t : Double, sigma : Double) => (randomGaussian() * sigma) + x_t
    for (i <- 1 to burnIn) {
      val x_p = propose(x, sigma)
      val acc = pdf(x_p) / pdf(x)
      if ( acc >= 1 || random0to1() < acc) {
        x = x_p
        rb.put(true)
      }
      else rb.put(false)
      val numAccepted = rb.numPositives()
      if (numAccepted < halfWindow) sigma /= 1.2
      else if (numAccepted > halfWindow) sigma *= 1.2
    }

    //  After burn-in period assume that the markov chain is ready for sampling
    val takeSample : Environment => Double =
      (environ : Environment) => {
        for (i <- 1 to stripe) {
          val x_p = propose(x, sigma)
          val acc = pdf(x_p) / pdf(x)
          if ( acc >= 1 || random0to1() < acc) {
            x = x_p
            rb.put(true)
          }
          else rb.put(false)
          val numAccepted = rb.numPositives()
          if (numAccepted < halfWindow) sigma /= 1.2
          else if (numAccepted > halfWindow) sigma *= 1.2
        }
        x
      }

    new PNum(takeSample)
  }

}
