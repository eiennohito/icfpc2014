package jp.ac.kyotou.kansai

import org.scalatest.{Matchers, FreeSpec}

class CodeGenTest extends FreeSpec with Matchers {
  "collectLocalVars" - {
    "with no argument" in {
      var ast = List (
        Assign("x", Literal(1)),
        Assign("y", Literal(2)),
        Statement(Plus(Reference("x"), Reference("y")))
      )

      var variables = CodeGen.collectLocalVars(ast, List())
      variables should have length (2)
    }

    "with arguments" in {
      var ast = List (
        Assign("x", Literal(1)),
        Statement(Plus(Reference("x"), Reference("y")))
      )

      var variables = CodeGen.collectLocalVars(ast, List("y"))
      variables should have length(1)
    }

    "Issue #6" in {
      var ast = List(
        Assign("x", Literal(1)),
        Statement(
          IfExpression(Equals(Literal(1), Literal(2)),
            List(Assign("y", Literal(2))),
            List(Assign("z", Literal(3))))
        ))
      var variables = CodeGen.collectLocalVars(ast, List("a"))
      variables should have length(3)
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
    }

    "mod" in {
      /*
       func mod(a, b) = {
         a - (a/b)*b
       }
       */
      var ast = FunctionDefiniton("mod", List("a", "b"),
        List(Statement(
          Minus(
            Reference("a"),
            Multiply(
              Divide(Reference("a"), Reference("b")),
              Reference("b"))))))

      var code = CodeGen.emitStructure(ast, NameGen())
      code should equal (List(
        Label("func_mod"),
        LoadFL("body_mod"),
        AppT(0),
        Label("body_mod"),
        Ld(1, 0),
        Ld(1, 0),
        Ld(1, 1),
        Arith("DIV"),
        Ld(1, 1),
        Arith("MUL"),
        Arith("SUB")
      ))
    }

    "local vars in if expression" in {
      /*
       def f() = {
         if (1 == 1) {
           var e = 10
           return e + 1
         } else {
           return 2
         }
       }
       */
      var ast = FunctionDefiniton("f", List[String](), List(
        Return(IfExpression(Equals(Literal(1), Literal(1)),
          List(Assign("e", Literal(10)), Return(Plus(Reference("e"), Literal(1)))), // true branch
          List(Return(Literal(2)))))))
      var code = CodeGen.emitStructure(ast, NameGen())
      code should equal(List(
        Label("func_f"), Ldc(0), LoadFL("body_f"), AppT(1), Label("body_f"),
        Label("if1"), Ldc(1), Ldc(1), Comp("CEQ"), SelTL("true2", "false3"),
        Label("true2"), Ldc(10), St(0, 0), Ld(0, 0), Ldc(1), Arith("ADD"), Ret(),
        Ldc(1), SelTL("after4", "terminate"), Label("false3"), Ldc(2), Ret(),
        Label("after4"), Ret()))
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

    "with consective labels" in {
      var code = List(
        Cons(),
        Label("label1"),
        Label("label2"),
        Label("label3"),
        Car(),
        Cdr(),
        SelTL("label1", "label2")
      )

      var derefCode = CodeGen.dereferenceLabels(code)
      derefCode should have length (4)
      derefCode should not contain (SelTL("label1", "label2"))
      derefCode should contain (SelTA(1, 1))
    }
  }

  "NameGenerator" - {
    "yields a fresh name" in {
      var gen = NameGen()
      var name1 = gen.get()
      var name2 = gen.get()
      name1 should not equal (name2)
    }
  }

  "emitCode" - {
    "Assign" in {
      /*
       var a = 1 + 2
       */
      var ast = Assign("a", Plus(Literal(1), Literal(2)))
      var code = CodeGen.emitCode(ast, Map("a" -> (1, 0)), NameGen())
      code should equal (List(
        Ldc(1), Ldc(2), Arith("ADD"), St(1, 0)
      ))
    }

    "Return" in {
      /*
       return 1 + 2
       */
      var ast = Return(Plus(Literal(1), Literal(2)))
      var code = CodeGen.emitCode(ast, Map(), NameGen())
      code should equal (List(
        Ldc(1), Ldc(2), Arith("ADD"), Ret()
      ))
    }

    "WhileStatement" in {
      /*
       while (1 < 2) {
         a = a + 1
       }
       */
      var ast = WhileStatement(Lesser(Literal(1), Literal(2)),
        List(Assign("a", Plus(Reference("a"), Literal(1)))))
      var code = CodeGen.emitCode(ast, Map("a" -> (1, 0)), NameGen())
      code should equal (List(
        Label("while1"), Ldc(2), Ldc(1), Comp("CGT"),
        SelTL("while_body2", "while_end3"), Label("while_body2"),
        Ld(1, 0), Ldc(1), Arith("ADD"), St(1, 0),
        Ldc(0), Ldc(0), Comp("CEQ"), SelTL("while1", "terminate"), Label("while_end3")))
    }
  }

  "emitExpr" - {
    "FunCall" in {
      var ast = FunCall("mod",
        List(Plus(Literal(1), Reference("a")), Minus(Reference("b"), Literal(3))))

      var code = CodeGen.emitExpr(ast, Map("a" -> (0, 0), "b" -> (0, 1)), NameGen())
      code should equal (List(
        Ldc(1), Ld(0, 0), Arith("ADD"),
        Ld(0, 1), Ldc(3), Arith("SUB"),
        LoadFL("func_mod"), App(2)))
    }


    "List" in {
      // (Cons "a" (Cons "b" 1))
      var ast = ConsAst(Reference("a"), ConsAst(Reference("b"), Literal(1)))

      var code = CodeGen.emitExpr(ast, Map("a" -> (0, 1), "b" -> (0, 2)), NameGen())
      code should equal (List(
        Ld(0, 1),
        Ld(0, 2),
        Ldc(1),
        Cons(),
        Cons()
      ))
    }

    "IsAtom()" in {
      // isAtom(MyList(1, 2))
      var ast = IsAtom(ConsAst(Literal(1), ConsAst(Literal(2), Literal(0))))
      var code = CodeGen.emitExpr(ast, Map(), NameGen())
      code should equal(List(
        Ldc(1), Ldc(2), Ldc(0), Cons(), Cons(), Atom()
      ))
    }

    "Debug()" in {
      // Debug(a)
      var ast = Debug(Reference("a"))
      var code = CodeGen.emitExpr(ast, Map("a" -> (1, 0)), NameGen())
      code should equal (List(
        Ld(1, 0), Dbug()
      ))
    }

    "UnaryMinus, -x" in {
      // -(1 + 2)
      var ast = UnaryMinus(Plus(Literal(1), Literal(2)))
      var code = CodeGen.emitExpr(ast, Map(), NameGen())
      code should equal (List(
        Ldc(0), Ldc(1), Ldc(2), Arith("ADD"), Arith("SUB")
      ))
    }

    "IfExpression" in {
      /*
       if 0 == 1 then 2 else 3
       */
      var ast = IfExpression(
        Equals(Literal(0), Literal(1)),
        List(Statement(Literal(2))),
        List(Statement(Literal(3))))

      var code = CodeGen.emitExpr(ast, Map(), NameGen())
      code should equal (List(
        Label("if1"), Ldc(0), Ldc(1), Comp("CEQ"), SelTL("true2", "false3"),
        Label("true2"), Ldc(2), Ldc(1),
        SelTL("after4", "terminate"), Label("false3"),
        Ldc(3), Label("after4")))
    }

    "IfExpression2" in {
      /*
       var a = if 0 == 1 then 3 else 4
       */
      var ast = Assign("a", IfExpression(
        Equals(Literal(0), Literal(1)),
        List(Statement(Literal(3))),
        List(Statement(Literal(4)))
      ))
      var code = CodeGen.emitCode(ast, Map("a" -> (0, 1)), NameGen())
      code should equal (List(
        Label("if1"), Ldc(0), Ldc(1), Comp("CEQ"), SelTL("true2", "false3"),
        Label("true2"), Ldc(3), Ldc(1),
        SelTL("after4", "terminate"), Label("false3"),
        Ldc(4), Label("after4"), St(0, 1)))
    }
  }
}
