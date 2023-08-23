package compiler.expression

import compiler.AST.*
import compiler.{ExpressionParser, TestUtil}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable.ArrayBuffer

class NameParserTest extends AnyFlatSpec with should.Matchers {

    behavior of "Name parser"

  it should "Should parse names" in {
    TestUtil.check("Example", ExpressionParser.nameParser) shouldBe Name("Example")
  }

}
