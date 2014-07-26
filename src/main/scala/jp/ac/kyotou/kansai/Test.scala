package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-25
 */
@gccCode
class Something {
  def func(i: Int): Int = {
    val pos = 5
    val next = pos + i

    return next
  }

}

object Something {
  val asts: Map[String, Any] = ???
}
