package jp.ac.kyotou.kansai

@gccCode
class RandomAccessList extends Support {
  // This is an implementation of BinaryRandomAccessList
  /*
   cons_RAL, head_RAL, tail_RAL: O(log n)
   lookup_RAL, update_RAL, size_RAL: O(log n)
   */

  case class Tree(x: Int, ch: MyList[Tree])
  def leaf_RAL(v: Int) = Tree(v, MyNil)
  def isLeaf_RAL(v: Tree) = v.ch == MyNil
  def node_RAL(v: Int, l: Tree, r: Tree) = Tree(v, MyList(l, r))
  def leftChild_RAL(t: Tree) = t.ch.car
  def rightChild_RAL(t: Tree) = t.ch.cdr.car

  case class Digit(d: Int, t: Tree)
  // type RList = MyList[Digit]
  def isEmpty_RAL(l: MyList[Digit]) = l == MyNil
  def zero_RAL(): Digit = Digit(0, leaf_RAL(-100))

  def sizeTree_RAL(t: Tree): Int = {
    if (isLeaf_RAL(t)) {
      return 1
    } else {
      return t.x
    }
  }
  def link_RAL(t1: Tree, t2: Tree): Tree = Tree(sizeTree_RAL(t1) + sizeTree_RAL(t2), MyList(t1, t2))
  def consTree_RAL(t: Tree, l: MyList[Digit]): MyList[Digit] = {
    if (isEmpty_RAL(l)) {
      return MyList(Digit(1, t))
    } else {
      if (l.car.d == 0) {
        return MyCons(Digit(1, t), l.cdr)
      } else {
        return MyCons(zero_RAL(), consTree_RAL(link_RAL(t, l.car.t), l.cdr))
      }
    }
  }

  def unconsTreeAux_RAL(t: MyList[Digit]): (Tree, MyList[Digit]) = {
    var ts = unconsTree_RAL(t.cdr)
    return (leftChild_RAL(ts._1), MyCons(Digit(1, rightChild_RAL(ts._1)), ts._2))
  }

  def unconsTree_RAL(t: MyList[Digit]): (Tree, MyList[Digit]) = {
    var td: Digit = t.car
    if (td.d == 1) {
      if (t.cdr == MyNil) {
        // [One t]
        return (td.t, MyNil)
      } else {
        // [One t, ... ]
        return (td.t, MyCons(zero_RAL(), t.cdr))
      }
    } else {
      // [Zero, ... ]
      return unconsTreeAux_RAL(t)
    }
  }

  def cons_RAL(x: Int, ts: MyList[Digit]) = consTree_RAL(leaf_RAL(x), ts)
  def head_RAL(ts: MyList[Digit]) = unconsTree_RAL(ts)._1.x
  def tail_RAL(ts: MyList[Digit]) = unconsTree_RAL(ts)._2
  def size_RAL(ts: MyList[Digit]): Int = {
    if (isEmpty_RAL(ts)) return 0
    if (ts.car.d == 0) {
      return size_RAL(ts.cdr)
    } else {
      return sizeTree_RAL(ts.car.t) + size_RAL(ts.cdr)
    }
  }

  def lookupTree_RAL(i: Int, t: Tree): Int = {
    if (isLeaf_RAL(t)) {
      if (i == 0) {
        return t.x
      } else {
        // return -100000 instead of raise error!
        return -100000
      }
    } else {
      if (i < (t.x / 2)) {
        return lookupTree_RAL(i, t.ch.car)
      } else {
        return lookupTree_RAL(i - (t.x / 2), t.ch.cdr.car)
      }
    }
  }
  def updateTree_RAL(i: Int, v: Int, t: Tree): Tree = {
    if (isLeaf_RAL(t)) {
      if (i == 0) {
        return leaf_RAL(v)
      } else {
        return leaf_RAL(-1999999)
      }
    } else {
      if (i < (t.x / 2)) {
        return node_RAL(t.x, updateTree_RAL(i, v, leftChild_RAL(t)), rightChild_RAL(t))
      } else {
        return node_RAL(t.x, leftChild_RAL(t), updateTree_RAL(i - (t.x / 2), v, rightChild_RAL(t)))
      }
    }
  }

  def lookup_RAL(i: Int, ral: MyList[Digit]): Int = {
    if (ral.car.d == 0) {
      return lookup_RAL(i, ral.cdr)
    } else {
      if (i < sizeTree_RAL(ral.car.t)) {
        return lookupTree_RAL(i, ral.car.t)
      } else {
        return lookup_RAL(i - sizeTree_RAL(ral.car.t), ral.cdr)
      }
    }
  }

  def update_RAL(i: Int, v: Int, ral: MyList[Digit]): MyList[Digit] = {
    if (ral.car.d == 0) {
      return MyCons(zero_RAL(), update_RAL(i, v, ral.cdr))
    } else {
      if (i < sizeTree_RAL(ral.car.t)) {
        return MyCons(Digit(1, updateTree_RAL(i, v, ral.car.t)), ral.cdr)
      } else {
        return MyCons(Digit(1, ral.car.t), update_RAL(i - sizeTree_RAL(ral.car.t), v, ral.cdr))
      }
    }
  }

  def ent(): Int = {
    var tree: Tree = leaf_RAL(1)
    debug(tree)

    debug(cons_RAL(1, MyNil))
    debug(cons_RAL(2, cons_RAL(1, MyNil)))

    var ral: MyList[Digit] = cons_RAL(1, cons_RAL(2, cons_RAL(3, cons_RAL(4, MyNil))))
    debug(head_RAL(ral))
    debug(head_RAL(tail_RAL(ral)))
    debug(head_RAL(tail_RAL(tail_RAL(ral))))
    debug(head_RAL(tail_RAL(tail_RAL(tail_RAL(ral)))))
    debug(size_RAL(ral))
    ral = update_RAL(0, 5, ral)
    ral = update_RAL(1, 6, ral)
    ral = update_RAL(2, 7, ral)
    debug(head_RAL(ral))
    debug(head_RAL(tail_RAL(ral)))
    debug(head_RAL(tail_RAL(tail_RAL(ral))))
    debug(head_RAL(tail_RAL(tail_RAL(tail_RAL(ral)))))
    ral = cons_RAL(10, ral)
    debug(lookup_RAL(0, ral))
    debug(lookup_RAL(1, ral))
    debug(lookup_RAL(2, ral))
    debug(lookup_RAL(3, ral))
    debug(lookup_RAL(4, ral))
    debug(size_RAL(ral))
    return 0
  }
}

import java.io.PrintWriter

object RandomAccessList extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var c = new RandomAccessList()
    c.ent()

    var code = Linker.compileAndLink(cleanAsts, "ent")
    //println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))

    var p = new PrintWriter("code.txt")
    p.println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }
}
