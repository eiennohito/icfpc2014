package jp.ac.kyotou.kansai

import org.scalatest.{Matchers, FreeSpec}

/**
 * @author eiennohito
 * @since 2014-07-26
 */
class SimpleMacroTest extends FreeSpec with Matchers {
  "macro ast transformer" - {
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

    "func9 should work with lists" in {
      val data = Something.cleanAsts.get("func9")

      data should not be (None)
      val expected = FunctionDefiniton("func9",List(),
        List(
          Assign("list",ConsAst(Literal(1),ConsAst(Literal(2),Literal(0)))),
          Assign("a",CarAst(Reference("list"))),
          Assign("b",CarAst(CdrAst(Reference("list")))),
          Return(Plus(Reference("a"),Reference("b"))))
      )

      data.get should be (expected)
    }

    "lstSum should not have any application" in {
      val data = Something.cleanAsts.get("lstSum")

      data should not be (None)

      val expected = FunctionDefiniton("lstSum", List("lst"),
        List(
          IfStatement(
            Equals(CdrAst(Reference("lst")), Literal(0)),
            List(Return(Literal(0))),
            List()),
          Return(Plus(
            CarAst(Reference("lst")),
            FunCall("lstSum",List(CdrAst(Reference("lst"))))))))
      data.get should be (expected)
    }
  }

  "list of tuples" in {
    val data = Something.cleanAsts.get("listTuple")
    data should not be (None)
    data.toString should not contain("MyNil")
  }

}
