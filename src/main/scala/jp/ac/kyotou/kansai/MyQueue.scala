package jp.ac.kyotou.kansai

@gccCode
class MyQueue extends Support {
  def rev_aux[T](l: MyList[T], r: MyList[T]): MyList[T] = {
    if (l == MyNil) return r
    return rev_aux(l.cdr, MyCons(l.car, r))
  }
  def rev[T](q: MyList[T]): MyList[T] = rev_aux(q, MyNil)

  case class Queue[T](f: MyList[T], b: MyList[T])

  def empty[T](): Queue[T] = Queue[T](MyNil, MyNil)
  def isEmpty[T](q: Queue[T]) = q.f == MyNil

  def checkf[T](q: Queue[T]): Queue[T] = {
    if (q.f == MyNil) {
      return Queue[T](rev(q.b), MyNil)
    } else {
      return q
    }
  }

  def push[T](q: Queue[T], x: T): Queue[T] = checkf(Queue[T](q.f, MyCons(x, q.b)))
  def pop[T](q: Queue[T]): (T, Queue[T]) = (q.f.car, checkf(Queue[T](q.f.cdr, q.b)))
  def head[T](q: Queue[T]): T = q.f.car
  def tail[T](q: Queue[T]): Queue[T] = checkf(Queue[T](q.f.cdr, q.b))

  def ent(): Int = {
    var q: Queue[Int] = empty()
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
