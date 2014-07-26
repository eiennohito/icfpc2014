package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-25
 */
@gccCode
class Something {
  def func(i: Int): Int = {
    val pos = 5
    val next = func2(pos, 42)
    return next
  }

  def func2(i1: Int, i2: Int) = i1 + i2

}

object Something {
  val asts: Map[String, Any] = ???
}
