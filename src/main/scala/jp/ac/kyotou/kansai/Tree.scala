package jp.ac.kyotou.kansai

@gccCode
class Tree extends Support {
  case class Tree(x: Int, children: MyList[Tree])
  def leaf(v: Int): Tree = Tree(v, MyNil)
  def node(v: Int, l: Tree, r: Tree) = Tree(v, MyList(l, r))
  def isLeaf(v: Tree) = v.children == MyNil
  def left(t: Tree): Tree = t.children.car
  def right(t: Tree): Tree = t.children.cdr.car
  def value(t: Tree): Int = t.x

  def ent(): Int = {
    var t = Tree(1, MyList(Tree(2, MyNil)))
    debug(t)
    debug(value(t))
    debug(value(left(t)))
    debug(isLeaf(t))
    debug(isLeaf(left(t)))
    debug(leaf(5))

    t = node(1, node(2, leaf(3), leaf(4)), leaf(5))
    debug(t)
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
