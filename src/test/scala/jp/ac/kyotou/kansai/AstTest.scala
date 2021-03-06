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
        Assign("z", Plus(Reference("x", "Int"), Reference("y", "Int")))
      )
    }
  }

  "code whatever" - {
    "whatever" in {
      // sum
      var code = List[Code] (
        Ldc(10),
        LoadFA(5),
        App(1),
        Pop(),
        Ret(),
        Ld(0, 0),
        Ldc(0),
        Comp("CEQ"),
        SelTA(9, 11),
        Ldc(0),
        Ret(),
        Ld(0, 0),
        Ld(0, 0),
        Ldc(1),
        Arith("SUB"),
        LoadFA(5),
        App(1),
        Arith("ADD"),
        Ret()
      )

      code.map(c => CodeGen.show(c)).mkString("\n")
    }
  }
}
