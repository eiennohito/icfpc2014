package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */

sealed trait MyList[+T] {}
case object MyNil extends MyList[Nothing]
case class MyCons[+T](car: T, cdr: MyList[T]) extends MyList[T]
