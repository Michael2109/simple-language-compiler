package compiler

import fastparse.*
import NoWhitespace.*

@main
def main(): Unit = {

 /* val Parsed.Success(15, _) = parse("(1+1*2)+3*4", expr(_))
  val Parsed.Success(21, _) = parse("((1+1*2)+(3*4*5))/3", expr(_))
  val Parsed.Failure(expected, failIndex, extra) = parse("1+1*", expr(_))
  val longAggMsg = extra.trace().longAggregateMsg
  assert(
    failIndex == 4,
    longAggMsg ==
      """Expected expr:1:1 / addSub:1:1 / divMul:1:3 / factor:1:5 / (number | parens):1:5, found """""
  )*/
}