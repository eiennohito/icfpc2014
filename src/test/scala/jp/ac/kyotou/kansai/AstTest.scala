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
        Assign("z", Application("+", Reference("x"), List(Reference("y"))))
      )
    }
  }

}
