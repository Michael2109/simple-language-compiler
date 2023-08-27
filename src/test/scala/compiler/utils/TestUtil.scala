package compiler.utils

import fastparse.*
import fastparse.NoWhitespace.*

import scala.util.Failure

object TestUtil {

  def check(text: String, parser: P[_] => P[Any]) = {

    parse(text, parser(_)) match
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed parsing:" + a + ":" + b + ":" + c.index + ":" + c.input + ":" + c.startIndex + ":" + c.stack)

  }

}
