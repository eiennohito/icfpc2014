package jp.ac.kyotou.kansai

import org.scalatest.{Matchers, FreeSpec}

class CodeGenTest extends FreeSpec with Matchers {
  "collectLocalVars" - {
    "with no argument" in {
      var ast = List (
        Assign("x", Literal(1)),
        Assign("y", Literal(2)),
        Expression(Plus(Reference("x"), Reference("y")))
      )

      var variables = CodeGen.collectLocalVars(ast, List())
      variables should have length (2)
    }

    "with arguments" in {
      var ast = List (
        Assign("x", Literal(1)),
        Expression(Plus(Reference("x"), Reference("y")))
      )

      var variables = CodeGen.collectLocalVars(ast, List("y"))
      variables should have length(1)
    }
  }

  "emitStructure" - {
    "function definition" in {
      /*
       func f() {
         var x = 1
       }
       */
      var ast = FunctionDefiniton("f", List[String](), List(Assign("x", Literal(1))))

      var code = CodeGen.emitStructure(ast)
      println(code.map(CodeGen.show).mkString("\n"))
    }
  }
}
