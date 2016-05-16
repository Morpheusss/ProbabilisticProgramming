package util

/**
 * Created by Morpheusss on 15/11/6.
 * Class: Function tools
 */
object Functools {

  //  From Scala documentation @ http://docs.scala-lang.org/zh-cn/overviews/core/implicit-classes.html
  implicit class IntWithTimes(x: Int) {
    def times[A](f: => A): Unit = {
      def loop(current: Int): Unit =
        if(current > 0) {
          f
          loop(current - 1)
        }
      loop(x)
    }
  }

}
