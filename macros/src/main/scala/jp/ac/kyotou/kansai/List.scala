package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */

sealed trait MyList[+T] {
  def car: T
  def cdr: MyList[T]
  def at(x: Int): T
}

object MyList {
  def apply[T](objs: T*): MyList[T] = {
    val lst = objs.toList
    mkList(lst)
  }

  private def mkList[T](objs: List[T]): MyList[T] = {
    objs match {
      case Nil => MyNil
      case x :: xs => MyCons(x, mkList(xs))
    }
  }
}


case object MyNil extends MyList[Nothing] {
  override def car = ???
  override def cdr = ???
  def at(x: Int) = ???
}
case class MyCons[+T](car: T, cdr: MyList[T]) extends MyList[T] {
  def at(x: Int): T = if (x == 0) car else cdr.at(x - 1)
}
