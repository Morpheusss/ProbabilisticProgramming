package util

/**
 * Created by Morpheusss on 15/11/5.
 * Class: Ring buffer of boolean variables
 * Intention(s): Building part of mcmc.
 * Parameter(s): Length
 */
class BoolRingBuffer (size : Int) extends IndexedSeq[Boolean]{

  if (size <= 0) throw new IllegalArgumentException("Ring buffer must have length at least 1!")

  private val ring : Array[Boolean] = Array.fill[Boolean](size)(false)
  private var curIndex : Int = 0

  def put(data : Boolean) : Unit = {
    ring(curIndex) = data
    curIndex = (curIndex + 1) % size
  }

  def remove() : Boolean = {
    var prevIndex = curIndex - 1
    if (prevIndex < 0) prevIndex += size
    ring(prevIndex)
  }

  def numPositives() : Int = ring.count((b : Boolean) => b)

  override def length: Int = size

  override def apply(idx: Int): Boolean = ring(idx)
}
