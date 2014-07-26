package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-25
 */
@gccCode
class Something extends Support {
  def func(i: Int): Int = {
    val pos = 5
    val next = func2(pos, 42)
    return next
  }

  def func2(i1: Int, i2: Int) = i1 + i2

  def func3: Int = {
    return 1 + 4
  }

  def func4: Int = {
    var i = 4
    i = i + 3
    return i - 1
  }

  def func5: Int = {
    if (0 == 0) {
      return 1
    }
    return 0
  }

  def func6: Int = {
    var i = 0
    while(i < 2) {
      i = i + 1
    }
    return i
  }

  def func7: Int = {
    val tpl = (1, 2, 3)
    return tpl._2
  }

  def func8: Int = {
    val x = (1, 2, 3)
    val a = x._1
    val b = tupleLast(x, 3)
    return a + b
  }

  def func9: Int = {
    val list = MyCons(1, MyCons(2, MyNil))
    val a = list.car
    var b = list.cdr.car
    return a + b
  }

}

object Something extends AstCleanup {
  val asts: Map[String, StructureAst] = ???
}
