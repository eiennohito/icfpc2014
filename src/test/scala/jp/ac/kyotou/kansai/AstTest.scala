package jp.ac.kyotou.kansai

import org.scalatest.{Matchers, FreeSpec}

/**
 * @author eiennohito
 * @since 2014-07-26
 */
class AstTest extends FreeSpec with Matchers {

  "ast whaterver" - {
    "whaterver" in {
      val ast = List (
        Assign("x", Literal(1)),
        Assign("y", Literal(2)),
        Assign("z", Plus(Reference("x"), Reference("y")))
      )
    }
  }

  "code whatever" - {
    "whatever" in {
      // sum
      var code = List[Code] (
        Ldc(10),
        LoadF(5),
        App(1),
        Pop(),
        Return(),
        Ld(0, 0),
        Ldc(0),
        Comp("CEQ"),
        SelT(9, 11),
        Ldc(0),
        Ret(),
        Ld(0, 0),
        Ld(0, 0),
        Ldc(1),
        Arith("SUB"),
        LoadF(5),
        App(1),
        Arith("ADD"),
        Ret()
      )

      println(code.map(c => c.show()).mkString("\n"))
    }
  }

  "emit" - {
    "whatever" in {
      var expr = Plus(Literal(1), Literal(2))
      println(CodeGen.emitExpr(expr))
    }
  }
}
