package jp.ac.kyotou.kansai

@gccCode
class Polymorphic extends Support {
  case class Foo[T](x: T, y: T)

  def fst[T](f: Foo[T]): T = f.x
  def snd[T](f: Foo[T]): T = f.y

  def ent(): Int = {
    var f = Foo[Int](1, 2)
    debug(fst(f))
    debug(snd(f))
    var g = Foo[MyList[Int]](MyList(1, 2), MyList(3, 4))
    debug(fst(g))
    debug(snd(g))
    return 0
  }
}

import java.io.PrintWriter

object Polymorphic extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var c = new Polymorphic()
    c.ent()

    var code = Linker.compileAndLink(cleanAsts, "ent")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))

    var p = new PrintWriter("code.txt")
    p.println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }
}
