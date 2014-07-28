package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-28
 */

@gccCode
class PatternMatchingTestfield {
  def constants(x: Int) = {
    x match {
      case 1 => 0
      case 0 => 1
      case _ => 5
    }
  }

  def capture(x: Int) = {
    x match {
      case y => y + 2
    }
  }

  case class XAst(x: Int, y: Int)

  def destruct(p: XAst) = {
    p match {
      case XAst(x, _) => x + 2
    }
  }

  def destructTuple(p: (Int, Int)) = p match {
    case (2, x) => x + 3
    case _ => 0
  }

  def variants(p: Int) = p match {
    case 1 | 2 | 3 | 4 => 0
    case _ => 10
  }

  def bools(x: Boolean) = x match {
    case true => 5
    case false => 2
  }
}

object PatternMatchingTestfield extends AstCleanup {
  val asts = ???
}
