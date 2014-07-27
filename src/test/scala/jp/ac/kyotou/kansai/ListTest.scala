package jp.ac.kyotou.kansai

import org.scalatest.{FreeSpec, Matchers}

/**
 * @author eiennohito
 * @since 2014-07-27
 */
class ListTest extends FreeSpec with Matchers {

  "MyList" - {
    "is created using sugar function" in {
      val lst = MyList(1, 2, 3)
      lst.car should be (1)
      lst.cdr.car should be (2)
      lst.cdr.cdr.cdr should be (MyNil)
    }

    "sugar for at function works" in {
      val lst = MyList(1, 2, 3, 4, 5, 6)
      lst.at(5) should be (6)
    }
  }

}
