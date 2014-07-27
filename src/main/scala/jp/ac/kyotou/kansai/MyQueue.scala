package jp.ac.kyotou.kansai

@gccCode
class MyQueue extends Support {
  def rev_aux(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
    if (l == MyNil) return r
    return rev_aux(l.cdr, MyCons(l.car, r))
  }
  def rev(q: MyList[Int]): MyList[Int] = rev_aux(q, MyNil)

  case class MyQueue(f: MyList[Int], b: MyList[Int])

  def empty(): MyQueue = {
    return MyQueue(MyNil, MyNil)
  }
  def isEmpty(q: MyQueue) = q.f == MyNil

  def checkf(q: MyQueue): MyQueue = {
    if (q.f == MyNil) {
      return MyQueue(rev(q.b), MyNil)
    } else {
      return q
    }
  }

  def push(q: MyQueue, x: Int): MyQueue = checkf(MyQueue(q.f, MyCons(x, q.b)))

  def pop(q: MyQueue): (Int, MyQueue) = {
    return (q.f.car, checkf(MyQueue(q.f.cdr, q.b)))
  }
  def head(q: MyQueue): Int = q.f.car
  def tail(q: MyQueue): MyQueue = checkf(MyQueue(q.f.cdr, q.b))

  def ent(): Int = {
    var q: MyQueue = empty()
    q = push(q, 1)
    q = push(q, 2)
    q = push(q, 3)
    debug(head(q))
    q = tail(q)
    debug(head(q))
    q = tail(q)
    debug(head(q))
    q = tail(q)
    debug(isEmpty(q))
    return 0
  }
}

import java.io.PrintWriter

object MyQueue extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    val code = Linker.compileAndLink(cleanAsts, "ent")
    var p = new PrintWriter("code.txt")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
    p.println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }
}
