package compiler.expression

import compiler.AST.IntConst
import compiler.{ExpressionParser, TestUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class NumberParserTest extends AnyFlatSpec with should.Matchers {

  "A number parser" should "parse integers" in {
    TestUtil.check("100", ExpressionParser.expressionParser) shouldBe IntConst(100)
  }
}