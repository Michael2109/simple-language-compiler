package compiler.expression

import compiler.AST.{Identifier, IntConst, Name, Negate, NestedExpr}
import compiler.{ExpressionParser, TestUtil}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable.ArrayBuffer

class IdentifierParserTest extends AnyFlatSpec with should.Matchers {

    behavior of "Identifier parser"

  it should "Should parse integers" in {
    TestUtil.check("x", ExpressionParser.expressionParser) shouldBe Identifier(Name("x"))
  }

}
