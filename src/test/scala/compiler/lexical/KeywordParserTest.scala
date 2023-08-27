package compiler.lexical

import compiler.ast.Ast.*
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable.ArrayBuffer

import fastparse._, NoWhitespace._
import compiler.parser.{ExpressionParser, LexicalParser}
import compiler.utils.TestUtil

import scala.util.Failure

class KeywordParserTest extends AnyFlatSpec with should.Matchers {

    behavior of "Keyword parser"

  it should "Should parse keywords" in {

   println( parse("class", LexicalParser.keyword("class")) match
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed parsing:" + a + ":" + b + ":" + c.index + ":" + c.input + ":" + c.startIndex + ":" + c.stack)
   )
  }

}
