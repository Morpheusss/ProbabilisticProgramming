package distributions.discrete

import definitions.PNum
import distributions.distributions._

import scala.annotation.tailrec

class DiscreteValues (outcomes : Map[Double, Double])
extends PNum (
  _ => {
    @tailrec
    def generate (x : Double, it : Iterator[Double]) : Double =
      if (it.hasNext) {
        val curVal = it.next()
        if (x <= outcomes(curVal)) curVal
        else generate(x-outcomes(curVal), it)
      }
      else Double.NaN

    generate(random0to1(), outcomes.keysIterator)
  }
){/* Nothing here */}
