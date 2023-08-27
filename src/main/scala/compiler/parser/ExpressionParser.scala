package compiler.parser

import compiler.parser.LexicalParser.*
import compiler.ast.Ast
import compiler.ast.Ast.*
import fastparse.*
import fastparse.SingleLineWhitespace.*


object ExpressionParser {

  def accessModifier[$: P]: P[Modifier] = P(keyword("protected")).map(_ => Protected()) | P(keyword("private")).map(_ => Private()) | P(keyword("local")).map(_ => PackageLocal())

  def annotationParser[$: P]: P[Annotation] = P("@" ~ nameParser).map(Annotation.apply)

  def allExpressionsParser[$: P] = P(methodCallParser | newClassInstanceParser | ternaryParser | numberParser | identifierParser | stringLiteral | parensParser)

  def parensParser[$: P]: P[Expression] = P("(" ~ (expressionParser) ~ ")")

  def termParser[$: P]: P[Expression] = P(Chain(allExpressionsParser, P(multiply | divide)))

  // TODO def negate[$: P]: P[Negate] = P("-" ~/ Chain(termParser, add | subtract)).map(Negate)
  def arith_exprParser[$: P]: P[Expression] = P(Chain(termParser, add | subtract))

  def rExprParser[$: P]: P[Expression] = P(Chain(arith_exprParser, LtE | Lt | GtE | Gt))

  def expressionParser[$: P]: P[Expression] = (P(Chain(rExprParser, and | or)).rep(min=1, sep = ".")).map(expressions => {
    expressions.length match {
      case 0 => BlockExpr(Seq())
      case 1 => expressions.head
      case _ => NestedExpr(expressions)
    }
  })

  def identifierParser[$: P]: P[Ast.Identifier] = identifier.map(x => Identifier(Name(x)))

  def finalModifierParser[$: P]: P[Ast.Final.type] = P("final").map(x => Final)

  def methodCallParser[$: P]: P[MethodCall] = P(nameParser ~ "(" ~ expressionParser.rep(sep = ",") ~ ")").map(x => MethodCall(x._1, x._2))

  def modifiers[$: P]: P[Seq[Modifier]] = P(accessModifier | typeModifier).rep

  def nameParser[$: P]: P[Name] = identifier.map(x => Name(x))

  def newClassInstanceParser[$: P]: P[NewClassInstance] = P(keyword("new") ~ typeRefParser ~ keyword("(") ~ expressionParser.rep(sep = ",") ~ keyword(")")).map(x => NewClassInstance(x._1, x._2, None))

  def numberParser[$: P]: P[Expression] = P(floatnumber ~ P("F" | "f")).map(FloatConst.apply) | P(longinteger).map(LongConst.apply) | P(floatnumber).map(DoubleConst.apply) | P(integer).map(IntConst.apply)

  def stringLiteral[$: P]: P[StringLiteral] = stringliteral.map(x => StringLiteral(x))

  def ternaryParser[$: P]: P[Ternary] = P(keyword("if") ~ expressionParser ~ "then" ~ expressionParser ~ "else" ~ expressionParser).map(x => Ternary(x._1, x._2, x._3))

  def typeModifier[$: P]: P[Modifier] = P(keyword("mutable")).map(_ => Final()) | P(keyword("abstract")).map(_ => Abstract()) | P(keyword("pure")).map(_ => Pure())

  def typeRefParser[$: P]: P[Type] = refParser.map(Type.apply)

  def refParser[$: P]: P[Ref] = P(identifier.rep(sep = ".", min = 2)).map(x => RefQual(QualName(NameSpace(x.dropRight(1)), x.last))) | P(identifier).map(RefLocal.apply)

  private def Chain[$: P](p: => P[Expression], op: => P[Ast.Operator]) = P(p ~ (op ~ p).rep).map {
    case (lhs, chunks) =>
      chunks.foldLeft(lhs) { case (lhs, (operator, rhs)) =>
        operator match {
          case op: ABinOp => ABinary(op, lhs, rhs)
          case op: BBinOp => BBinary(op, lhs, rhs)
          case op: RBinOp => RBinary(op, lhs, rhs)
        }
      }
  }

  def op[T, $: P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)

  def Lt[$: P] = P(op("<", Ast.Less))

  def Gt[$: P] = P(op(">", Ast.Greater.asInstanceOf[Operator]))

  def Eq[$: P] = P(op("==", Ast.Equal.asInstanceOf[Operator]))

  def GtE[$: P] = P(op(">=", Ast.GreaterEqual.asInstanceOf[Operator]))

  def LtE[$: P] = P(op("<=", Ast.LessEqual.asInstanceOf[Operator]))

  def comp_op[$: P] = P(LtE | GtE | Eq | Gt | Lt)

  def add[$: P] = P(op("+", Ast.Add.asInstanceOf[Operator]))

  def subtract[$: P] = P(op("-", Ast.Subtract.asInstanceOf[Operator]))

  def multiply[$: P] = P(op("*", Ast.Multiply.asInstanceOf[Operator]))

  def divide[$: P] = P(op("/", Ast.Divide.asInstanceOf[Operator]))

  def and[$: P] = P(op("&&", Ast.And.asInstanceOf[Operator]))

  def or[$: P] = P(op("||", Ast.Or.asInstanceOf[Operator]))

}