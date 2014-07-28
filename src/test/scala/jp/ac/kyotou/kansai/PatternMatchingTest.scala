package jp.ac.kyotou.kansai

import org.scalatest.{Matchers, FreeSpec}

/**
 * @author eiennohito
 * @since 2014-07-28
 */
class PatternMatchingTest extends FreeSpec with Matchers {
  "pattern matching" - {
    val asts = PatternMatchingTestfield.cleanAsts

    "dont have invalid asts" in {
      ForbiddenAsts.check(asts) should be (empty)
    }

    "simple pattern should have good ast" in {
      val data = asts.get("constants")
      data should not be (None)

      println(data)
    }

    "capture should introduce variable" in {
      val data = asts.get("capture")
      println(data)
    }

    "destruct should capture inner vars" in {
      val data = asts.get("destruct")
      println(data)
    }

    "destruct tuple and check value" in {
      val data = asts.get("destructTuple")
      println(data)
    }

    "varitants" in {
      val data = asts.get("variants")
      println(data)
    }
  }
}
