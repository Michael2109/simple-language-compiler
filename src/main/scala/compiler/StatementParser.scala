package compiler

import compiler.AST.*
import fastparse.*
import fastparse.NoWhitespace.*

import scala.collection.Seq
object StatementParser {

  def fileParser[$: P]: P[File] = P(nameSpaceParser ~ importParser.rep ~ modelParser.rep).map(x => File(ModuleHeader(x._1, x._2), x._3))


  def modelParser[$:P]: P[Model] = P(LexicalParser.keyword("class") ~/ space~ LexicalParser.identifier ~ ("extends" ~ ExpressionParser.typeRefParser).? ~ (LexicalParser.keyword("with") ~ ExpressionParser.typeRefParser).rep() ~ curlyBraceBlock).map(x => ClassModel(x._1, Seq(), Seq(), x._2, Seq(), x._3, x._4.statement)).log

  def space[$: P]         = P( CharsWhileIn(" \r\n", 0) )

  def newline[$: P]: P[Unit] = P("\n")
  def assignParser[$:P]: P[Assign] = P(("mutable").!.? ~space ~  ExpressionParser.nameParser ~ space ~ (":" ~/space ~  ExpressionParser.typeRefParser).? ~/ space ~ P(LexicalParser.keyword("=")) ~ block).map(x => Assign(x._2, x._3, x._1.isEmpty, x._4))

  def block[$:P]: P[Block] = P(space ~  (curlyBraceBlock | ExpressionParser.expressionParser.map(Inline)))

  def commentParser[$:P]: P[_] = P(LexicalParser.comment)

  def exprAsStmt[$:P]: P[Statement] = P( space ~ ExpressionParser.expressionParser ~ space).map(ExprAsStmt)


  def ifStatementParser[$:P]: P[If] = {
    def ifParser[$: P]: P[(Expression, Statement)] = P(LexicalParser.keyword("if") ~ space  ~/  "(" ~/ space ~ ExpressionParser.expressionParser ~ space ~ ")" ~ space ~/ block).map(x => (x._1, x._2))

    def elseParser[$: P]: P[Statement] = P(elifP ~ elseParser.?).map(x => If(x._1, x._2, x._3)) | P(elseP)

    def elifP[$: P]: P[(Expression, Statement)] = P(LexicalParser.keyword("else") ~ LexicalParser.keyword("if") ~/ "(" ~ ExpressionParser.expressionParser ~ ")" ~ block).map(x => (x._1, x._2))

    def elseP[$: P]: P[Statement] = P(LexicalParser.keyword("else") ~/ block).map(x => x)
    P(ifParser ~ elseParser.?).map(x => If(x._1, x._2, x._3)).log
  }

  def importParser[$:P]: P[Import] = P(LexicalParser.keyword("import") ~/ ExpressionParser.nameParser.rep(sep = ".")).map(Import)

  def fieldParser[$:P]: P[Field] = P(ExpressionParser.nameParser ~ ":" ~ ExpressionParser.typeRefParser).map(x => Field(x._1, x._2, None))

  def methodParser[$:P]: P[Statement] = P(ExpressionParser.modifiers  ~ space ~  ExpressionParser.nameParser ~space ~  "(" ~/ space ~ fieldParser.rep(sep = ",") ~space ~  ")" ~ (":" ~space ~  ExpressionParser.typeRefParser ~space).? ~ space ~ block).map(x => Method(x._2, Seq(), x._3, x._1, x._4, x._5))

  def nameSpaceParser[$:P]: P[NameSpace] = P(LexicalParser.keyword("package") ~/ ExpressionParser.nameParser.rep(sep = ".")).map(NameSpace)

  def reassignParser[$:P]: P[Reassign] = P(ExpressionParser.nameParser ~ "<-" ~/ block).map(x => Reassign(x._1, x._2))

//  def statementParser[$:P]: P[Statement] = P(!commentParser ~ (modelParser | ifStatementParser | methodParser | assignParser | reassignParser | exprAsStmt)).log

  def statementParser[$:P]: P[Statement] = P(!commentParser ~ (modelParser | ifStatementParser | methodParser | assignParser | reassignParser | exprAsStmt)).log
  def curlyBraceBlock[$:P]: P[CurlyBraceBlock] =  (space ~ "{" ~/ statementParser.rep(sep = space) ~ space~ "}" ~ space).map(x => CurlyBraceBlock(x)).log

}