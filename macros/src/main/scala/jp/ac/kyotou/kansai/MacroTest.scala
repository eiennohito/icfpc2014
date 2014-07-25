package jp.ac.kyotou.kansai

import scala.language.experimental.macros


trait Command
case class Value(name: String, value: Int) extends Command
case class Addition(left: Command, right: Command)

/**
 * @author eiennohito
 * @since 2014-07-25
 */
object MacroTest {
  def codeTest(code: Int): Int = macro MacroTestImpl.codeTest

  def code(code: Any): List[Command] = macro MacroTestImpl.code
}

object MacroTestImpl {
  import scala.reflect.macros.whitebox.Context
  def codeTest(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    q"(40 + $code)"
  }

  def code(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._
    var p: TermName = null

    println(code)

    val q"..$exprs" = code

    val res = exprs.collect {
      case q"val $name = ${res: Int}" => Value(name.decodedName.toString, res)
      //case q"$x + $y" => Addition(x.name, y.name)
    }

    val items = res.map {
      case Value(a, b) => q"Value($a, $b)"
    }

    q"List(..$items)"
  }
}
