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
        Return(Literal(5))
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
          Assign("a",CarAst(Reference("x","scala.Tuple3"))),
          Assign("b",CdrAst(CdrAst(CdrAst(Reference("x","scala.Tuple3"))))),
          Return(Plus(Reference("a","scala.Int"),Reference("b","scala.Int")))))
      data.get should be (expected)
    }

    "func9 should work with lists" in {
      val data = Something.cleanAsts.get("func9")

      data should not be (None)
      val expected = FunctionDefiniton("func9",List(),
        List(
          Assign("list",ConsAst(Literal(1),ConsAst(Literal(2),Literal(0)))),
          Assign("a",CarAst(Reference("list", "jp.ac.kyotou.kansai.MyCons"))),
          Assign("b",CarAst(CdrAst(Reference("list", "jp.ac.kyotou.kansai.MyCons")))),
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
            Equals(CdrAst(Reference("lst", "jp.ac.kyotou.kansai.MyList")), Literal(0)),
            List(Return(Literal(0))),
            List()),
          Return(Plus(
            CarAst(Reference("lst", "jp.ac.kyotou.kansai.MyList")),
            FunCall("lstSum",List(CdrAst(Reference("lst", "jp.ac.kyotou.kansai.MyList"))))))))
      data.get should be (expected)
    }
  }

  "list of tuples" in {
    val data = Something.cleanAsts.get("listTuple")
    data should not be (None)
    data.toString should not contain("MyNil")
  }

  "highOrderedFns" in {
    val data = Something.cleanAsts.get("highOrderFn")
    data should not be (None)

    val expected = FunctionDefiniton("highOrderFn",List("fn"),
      List(
        Return(
          FunCall("fn",List(Literal(2)),true))))
    data.get should equal(expected)
  }

}
