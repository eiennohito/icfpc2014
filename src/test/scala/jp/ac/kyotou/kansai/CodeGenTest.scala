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

      var code = CodeGen.emitStructure(ast, NameGen())
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

      var code = CodeGen.emitStructure(ast, NameGen())
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

  "list operators" - {
    "cons" in {
      // (Cons "a" (Cons "b" 1))
      var ast = ConsAst(Reference("a"), ConsAst(Reference("b"), Literal(1)))

      var code = CodeGen.emitExpr(ast, Map("a" -> (0, 1), "b" -> (0, 2)))
      code should equal (List(
        Ld(0, 1),
        Ld(0, 2),
        Ldc(1),
        Cons(),
        Cons()
      ))
    }
  }

  "name generator" - {
    "yields a fresh name" in {
      var gen = NameGen()
      var name1 = gen.get()
      var name2 = gen.get()
      name1 should not equal (name2)
    }
  }
}
