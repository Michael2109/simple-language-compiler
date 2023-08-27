package compiler.expression

import compiler.ast.Ast.IntConst
import compiler.parser.ExpressionParser
import compiler.utils.TestUtil
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class NumberParserTest extends AnyFlatSpec with should.Matchers {

  "A number parser" should "parse integers" in {
    TestUtil.check("100", ExpressionParser.expressionParser) shouldBe IntConst(100)
  }
}