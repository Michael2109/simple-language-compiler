package compiler.lexical

import compiler.ast.Ast.*
import compiler.parser.LexicalParser
import compiler.utils.TestUtil
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class LexicalParserTest extends AnyFlatSpec with should.Matchers {

  behavior of "Lexical parser"

  it should "Should parse identifiers" in {
    TestUtil.check("identifierName", LexicalParser.identifier) shouldBe "identifierName"
  }

}