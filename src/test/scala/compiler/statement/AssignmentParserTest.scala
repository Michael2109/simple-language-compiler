package compiler.statement

import compiler.ast.Ast.*
import compiler.parser.{LexicalParser, StatementParser}
import compiler.utils.TestUtil
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.{ArrayBuffer, Stack}

class AssignmentParserTest extends AnyFlatSpec with should.Matchers {

  behavior of "Assignment parser"

  it should "Should parse assignment statement" in {
    val code = "x = 10"

    TestUtil.check(code, StatementParser.statementParser) shouldBe
      Assign("x", None, true, Inline(IntConst(10)))
  }

}