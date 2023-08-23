package compiler.statement

import compiler.AST.*
import compiler.{LexicalParser, StatementParser, TestUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.{ArrayBuffer, Stack}

class IfStatementParserTest extends AnyFlatSpec with should.Matchers {

  behavior of "If statement parser"

  it should "Should parse if statement" in {
    val code = "if(true){ x }"

    TestUtil.check(code, StatementParser.statementParser) shouldBe
      If(Identifier(Name("true")), CurlyBraceBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))), None)
  }


  it should "Should parse if statement with multiple statements" in {
    val code =
      """if (true) {
        |  x
        |    y
        |z
        |}
     """.stripMargin
    TestUtil.check(code, StatementParser.statementParser) shouldBe
      If(Identifier(Name("true")), CurlyBraceBlock(List(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z"))))), None)
  }

}