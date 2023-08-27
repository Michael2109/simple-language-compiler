package compiler.expression

import compiler.ast.Ast.{Identifier, IntConst, Name, Negate, NestedExpr}
import compiler.parser.ExpressionParser
import compiler.utils.TestUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable.ArrayBuffer

class IdentifierParserTest extends AnyFlatSpec with should.Matchers {

    behavior of "Identifier parser"

  it should "Should parse integers" in {
    TestUtil.check("x", ExpressionParser.expressionParser) shouldBe Identifier(Name("x"))
  }

}
