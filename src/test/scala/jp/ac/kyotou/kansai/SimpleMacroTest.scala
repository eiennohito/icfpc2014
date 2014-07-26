package jp.ac.kyotou.kansai

import org.scalatest.{Matchers, FreeSpec}

/**
 * @author eiennohito
 * @since 2014-07-26
 */
class SimpleMacroTest extends FreeSpec with Matchers {
  "macro ast transformer" - {
    "works with a simple macro" in {
      MacroTest.codeTest(20) should equal (60)
    }

    "transforms ast to a list" in {
      val ast = MacroTest.code {
        val i = 20
      }

      ast should have length (1)
      ast.head should have (
        'name ("i"),
        'value (20)
      )
    }

    "transforms a weird thing" in {
      Something.asts.get("func") should not be (None)
      Something.asts.size should be (3)
    }

    "func3 should have rewritten AST" in {
      val ast = Something.cleanAsts.get("func3")

      val requred = FunctionDefiniton("func3", Nil, List(
        Return(
          Plus(Literal(1), Literal(4))
        )
      ))

      ast should not be (None)
      ast.get should be (requred)
    }

    "func8 should provide good ast for tuples" in {
      val data = Something.cleanAsts.get("func8")

      data should not be (None)
      val expected = FunctionDefiniton("func8",List(),
        List(
          Assign("x",ConsAst(Literal(1),ConsAst(Literal(2),Literal(3)))),
          Assign("a",CarAst(Reference("x"))),
          Assign("b",CdrAst(CdrAst(Reference("x")))),
          Return(Plus(Reference("a"),Reference("b")))))

      data.get should be (expected)
    }
  }

}
