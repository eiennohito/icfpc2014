package jp.ac.kyotou.kansai

import scala.collection.mutable

/**
 * @author eiennohito
 * @since 2014-07-26
 */
trait Support {
  def tupleLast[T1, T2](tpl: (T1, T2), len: Int): T2 = tpl._2
  def tupleLast[T1, T2, T3](tpl: (T1, T2, T3), len: Int): T3 = tpl._3
  def tupleLast[T1, T2, T3, T4](tpl: (T1, T2, T3, T4), len: Int): T4 = tpl._4
  def tupleLast[T1, T2, T3, T4, T5](tpl: (T1, T2, T3, T4, T5), len: Int): T5 = tpl._5

  def isInt(x: Int): Boolean = true
  def isInt[T](x: T): Boolean = false

  def debug(x: Any): Unit = println(x)
}

class MyArray[T] private (internal: mutable.Map[Int, T]) {
  def get(i: Int) = internal(i)
  def put(pos: Int, obj: T) = internal(pos) = obj
}

object MyArray {
  def apply[T](): MyArray[T] = {
    new MyArray[T](new mutable.HashMap[Int, T]())
  }
}
