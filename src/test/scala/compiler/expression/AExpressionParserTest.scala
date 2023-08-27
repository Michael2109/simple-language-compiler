package compiler.expression

import compiler.ast.Ast.*
import compiler.parser.ExpressionParser
import compiler.utils.TestUtil
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class AExpressionParserTest extends AnyFlatSpec with should.Matchers {

  behavior of "Expression parser - Arithmetic Expressions"

  it should "Should parse integers" in {
    TestUtil.check("1", ExpressionParser.expressionParser) shouldBe IntConst(1)
  }

  it should "Should parse negative"
  {
   // TODO TestUtil.check("-1", ExpressionParser.expressionParser) shouldBe Negate(IntConst(1))
  }

  it should "Should parse addition"
  {
    TestUtil.check("1 + 2", ExpressionParser.expressionParser) shouldBe ABinary(Add, IntConst(1), IntConst(2))
  }
  it should "Should parse subtract"
  {
    TestUtil.check("1 - 2", ExpressionParser.expressionParser) shouldBe ABinary(Subtract, IntConst(1), IntConst(2))
  }
  it should "Should parse multiply"
  {
    TestUtil.check("1 * 2", ExpressionParser.expressionParser) shouldBe ABinary(Multiply, IntConst(1), IntConst(2))
  }
  it should "Should parse divide"
  {
    TestUtil.check("1 / 2", ExpressionParser.expressionParser) shouldBe ABinary(Divide, IntConst(1), IntConst(2))
  }
  it should "Should parse mixed"
  {
    TestUtil.check("1 / 100 * 3 + 200 - 4", ExpressionParser.expressionParser) shouldBe ABinary(Subtract, ABinary(Add, ABinary(Multiply, ABinary(Divide, IntConst(1), IntConst(100)), IntConst(3)), IntConst(200)), IntConst(4))
  }
  it should "Should parse parentheses - 1"
  {
    TestUtil.check("1 / 100 * (2 + 200) - 3", ExpressionParser.expressionParser) shouldBe ABinary(Subtract, ABinary(Multiply, ABinary(Divide, IntConst(1), IntConst(100)), ABinary(Add, IntConst(2), IntConst(200))), IntConst(3))
  }
}