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
      Something.asts.get("test") should be (Some(10))
    }
  }

}
