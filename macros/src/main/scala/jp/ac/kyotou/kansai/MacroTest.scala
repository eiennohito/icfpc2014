package jp.ac.kyotou.kansai

import scala.language.experimental.macros


/**
 * @author eiennohito
 * @since 2014-07-25
 */
object MacroTest {
  def codeTest(code: Int): Int = macro MacroTestImpl.codeTest
}

object MacroTestImpl {
  import scala.reflect.macros.whitebox.Context
  def codeTest(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    q"(40 + $code)"
  }
}
