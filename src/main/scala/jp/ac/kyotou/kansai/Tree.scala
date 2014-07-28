package jp.ac.kyotou.kansai

@gccCode
class Tree extends Support {
  case class Tree[T](x: T, ch: MyList[Tree[T]])
  def leaf[T](v: T): Tree[T] = Tree[T](v, MyNil)
  def node[T](v: T, l: Tree[T], r: Tree[T]) = Tree[T](v, MyList(l, r))
  def isLeaf[T](v: Tree[T]) = v.ch == MyNil
  def left[T](t: Tree[T]): Tree[T] = t.ch.car
  def right[T](t: Tree[T]): Tree[T] = t.ch.cdr.car
  def value[T](t: Tree[T]): T = t.x

  def ent(): Int = {
    var t = node(1, node(2, leaf(3), leaf(4)), leaf(5))
    debug(t)
    debug(value(t))
    debug(value(left(t)))
    debug(isLeaf(t))
    debug(isLeaf(left(t)))
    debug(leaf(5))

    var tl = node((1, 2), leaf((3, 4)), leaf((5, 6)))
    debug(tl)
    debug(value(tl))
    debug(value(left(tl)))
    debug(value(right(tl)))
    return 0
  }
}

import java.io.PrintWriter

object Tree extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var code = Linker.compileAndLink(cleanAsts, "ent")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))

    var p = new PrintWriter("code.txt")
    p.println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }
}
