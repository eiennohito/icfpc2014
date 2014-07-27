package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-25
 */
@gccCode
class Something extends Support {

  //必ず返す型を示さなければなりません
  def smallest: Int = {
    return 0 //必ず return
  }

  def usableTypes: Int = {
    val a = 5 //Int
    val b = (1, 2) //Tuple
    val c = MyCons(2, MyNil) // Linked list
    return a + b._1 //必ず returnを書くこと
  }

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
    return tpl._1
  }

  def func8: Int = {
    val x = (1, 2, 3)
    val a = x._1
    val b = x._3 //タプルの最後のやつを読むと必ずこの関数を使う
    return a + b
  }

  def func9: Int = {
    val list = MyCons(1, MyCons(2, MyNil))
    val a = list.car
    var b = list.cdr.car
    return a + b
  }

  def lstSum(lst:MyList[Int]) : Int = {
    if (lst.cdr == MyNil) {
      return 0
    }
    return lst.car + lstSum(lst.cdr)
  }

  def usingIsAtom: Int = {
    val x = MyList(1, 2)
    val y = x != MyNil
    val k = MyNil == x
    val z = isInt(x)
    return 0
  }

  def listTuple(x: (MyList[Int], Int)): (MyList[Int], Int) = {
    return (MyCons(5, MyNil), 3)
  }

  def highOrderFn(fn: Int => Int): Int = {
    return fn(2)
  }

  def listSyntSugar: Int = {
    val list = MyList(1, 2, 3)
    val x = list.at(1)
    return x
  }

}

object Something extends AstCleanup {
  val asts: Map[String, StructureAst] = ???
}
