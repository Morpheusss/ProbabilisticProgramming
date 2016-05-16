package definitions

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Class: Environment for simulations
  * Intention(s): Resolving sampled values of variables
  *   while preserving dependencies.
  * Assumptions: simulations runs sequentially.
  */
class Environment (val mappings : mutable.HashMap[Long, Any] = mutable.HashMap[Long, Any](),
                   var next : Environment = null) {
  /*
  * This function pushes one more layer/scope on top of the current environment.
  * Mappings resolved before are still visible, but before call to endCurrent(),
  * any further changes are only made to the top scope.
  * This function is to be called at the beginning of each simulation.
  * */
  def extendNew = new Environment(mutable.HashMap[Long, Any](), this)

  /*
  * Reversed for extendNew(): pop the top scope.
  * This method is to be called at the end of each simulation.
  * */
  def endCurrent = next

  /*
  * This function tries to determine the value of a probabilistic variable.
  * If pVar has been sampled/resolved, return its bounded value.
  * Otherwise, sample it, bound it to the sampled value, and return the sample.
  * */
  def resolve[T] (pVar: PVar[T]) : T = {
    val (result, found) = searchValue(pVar.id)
    if (found) result.asInstanceOf[T]
    else {
      val s = pVar.sample(this)
      mappings += pVar.id -> s
      s
    }
  }

  @tailrec
  private def searchValue(id : Long) : (Any, Boolean) =
    if (mappings contains id) (mappings(id), true)
    else if (next == null) (null, false)
    else next.searchValue(id)

  /*
  * This function looks up the value of a PVar.
  * The given default value is returned if PVar not found.
  * */
  @tailrec
  final def find[T](pVar: PVar[T], default : T) : T =
    if (mappings contains pVar.id) mappings(pVar.id).asInstanceOf[T]
    else if (next == null) default
    else next.find(pVar, default)

  /*
  * This function returns whether there is a resolved value
  *  for the specified PVar
  * */
  @tailrec
  final def hasResolved (pVar: PVar[Any]) : Boolean =
    if (mappings contains pVar.id) true
    else if (next == null) false
    else next.hasResolved(pVar)

  /*
  * Create a deep copy of the environment
  * */
  override def clone : Environment =
    new Environment(
      mappings.clone,
      if (next == null ) null else next.clone
    )

  /*
  * Helper function: convert the whole environment list to string for printing.
  * */
  override def toString() : String = {
    val curStr = "{ " + mappings.mkString(", ") + " }"
    if (next == null) curStr
    else curStr + " ==> " + next.toString()
  }

}
