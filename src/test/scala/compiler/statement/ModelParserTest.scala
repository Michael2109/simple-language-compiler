package compiler.statement

import compiler.ast.Ast.*
import compiler.parser.{LexicalParser, StatementParser}
import compiler.utils.TestUtil
import fastparse.P
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.{ArrayBuffer, Stack}

import fastparse.*
import fastparse.NoWhitespace.*
class ModelParserTest extends AnyFlatSpec with should.Matchers {
  behavior of "Model parser"

  it should "parse an empty class" in {
    val code =
      "class test {}"
    TestUtil.check(code, StatementParser.modelParser ) shouldBe
      ClassModel("test", List(), List(), None, List(), List(), List())
  }

  it should "Should parse class with contents" in {
    val code =
      """class test {
        |
        |  x = 5
        |
        |  x(){
        |    y = 10
        |  }
        |
        |}
     """.stripMargin
    TestUtil.check(code, StatementParser.statementParser) shouldBe
      ClassModel("test", List(), List(), None, List(), List(), List(Assign("x", None, true, Inline(IntConst(5))), Method("x", List(), List(), List(), None, CurlyBraceBlock(List(Assign("y", None, true, Inline(IntConst(10))))))))
  }
}