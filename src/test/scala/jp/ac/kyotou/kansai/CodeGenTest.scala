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

    "mod" in {
      /*
       func mod(a, b) = {
         a - (a/b)*b
       }
       */
      var ast = FunctionDefiniton("mod", List("a", "b"),
        List(Expression(
          Minus(
            Reference("a"),
            Multiply(
              Divide(Reference("a"), Reference("b")),
              Reference("b"))))))

      var code = CodeGen.emitStructure(ast)
    }
  }

  "dereferenceLabels" - {
    "no label" in {
      var code = List(
        Cons(),
        Car(),
        Cdr(),
        Ret()
      )

      var derefCode = CodeGen.dereferenceLabels(code)
      derefCode should equal (code)
    }

    "with some labels" in {
      var code = List(
        Cons(),
        Label("label1"),
        LoadFL("label1"),
        Car(),
        Label("label2"),
        SelTL("label1", "label2"),
        Cdr()
      )

      var derefCode = CodeGen.dereferenceLabels(code)
      derefCode should have length (code.length - 2)
      derefCode should not contain (LoadFL("label1"))
      derefCode should contain (LoadFA(1))
      derefCode should not contain (SelTL("label1", "label2"))
      derefCode should contain (SelTA(1, 3))
    }
  }
}
