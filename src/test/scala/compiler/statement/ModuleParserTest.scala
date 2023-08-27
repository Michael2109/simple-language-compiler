package compiler.statement

import compiler.ast.Ast.*
import compiler.parser.{LexicalParser, StatementParser}
import compiler.utils.TestUtil
import fastparse.NoWhitespace.*
import fastparse.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.{ArrayBuffer, Stack}

class ModuleParserTest extends AnyFlatSpec with should.Matchers {
  behavior of "Module parser"

  it should "parse a module with contents" in {
    val code =
      """
        |package x.y.z
        |
        |import x.y.z
        |
        |class Test1 {
        |
        |}
        |
        |class Test2 {
        |
        |  let x = 5
        |
        |  let x(){
        |    let y = 10
        |  }
        |
        |}
     """.stripMargin
    TestUtil.check(code, StatementParser.moduleParser) shouldBe
      Module(NameSpace(List("x", "y", "z")), List(Import(List("x", "y","z"))), List(ClassModel("Test1", List(), List(), None, List(), List(), List()), ClassModel("Test2", List(), List(), None, List(), List(), List(Assign("x", None, true, Inline(IntConst(5))), Method("x", List(), List(), List(), None, CurlyBraceBlock(List(Assign("y", None, true, Inline(IntConst(10))))))))))
  }
}