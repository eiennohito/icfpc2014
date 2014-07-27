package jp.ac.kyotou.kansai

import org.scalatest.{Matchers, FreeSpec}

/**
 * @author eiennohito
 * @since 2014-07-27
 */
class AstCleanupTest extends FreeSpec with Matchers {
  "tuple extraction" - {
    val ast = Literal(0)
    val rewriter = new Rewriter(Map())
    "2 tuple <- right" in {
      val res = rewriter.selectTupleElementLen(ast, 2, 2)
      res should be (CdrAst(ast))
    }

    "2 tuple <- left" in {
      val res = rewriter.selectTupleElementLen(ast, 1, 2)
      res should be (CarAst(ast))
    }

    "3 tuple <- second" in {
      val res = rewriter.selectTupleElementLen(ast, 2, 3)
      res should be (CarAst(CdrAst(ast)))
    }

    "3 tuple <- third" in {
      val res = rewriter.selectTupleElementLen(ast, 3, 3)
      res should be (CdrAst(CdrAst(ast)))
    }
  }
}
