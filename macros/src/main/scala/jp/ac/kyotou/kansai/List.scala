package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */

sealed trait MyList[+T] {
  def car: T
  def cdr: MyList[T]
}
case object MyNil extends MyList[Nothing] {
  override def car = ???
  override def cdr = ???
}
case class MyCons[+T](car: T, cdr: MyList[T]) extends MyList[T]
