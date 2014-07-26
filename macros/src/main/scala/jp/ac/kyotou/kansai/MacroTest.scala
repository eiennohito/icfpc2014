package jp.ac.kyotou.kansai

import scala.language.experimental.macros


sealed trait MyList[+T] {

}

case object MyNil extends MyList[Nothing]
case class :*[+T](head: T) extends MyList[T]

case class AssignConst(name: String, value: Int)

/**
 * @author eiennohito
 * @since 2014-07-25
 */
object MacroTest {
  def codeTest(code: Int): Int = macro MacroTestImpl.codeTest

  def code(code: Any): List[CodeAst] = macro MacroTestImpl.code
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

    val res = code.collect {
      case q"val $name = ${res: Int}" => AssignConst(name.decodedName.toString, res)
      //case q"$x + $y" => Addition(x.name, y.name)
    }

    val items = res.map {
      case AssignConst(a, b) => q"Assign($a, Literal($b))"
    }

    q"List(..$items)"
  }
}
