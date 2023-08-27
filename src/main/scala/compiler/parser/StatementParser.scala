package compiler.parser

import compiler.ast.Ast.*
import compiler.parser.{ExpressionParser, LexicalParser}
import fastparse.*
import fastparse.NoWhitespace.*
object StatementParser {

  def moduleParser[$: P]: P[Module] = P(space ~nameSpaceParser ~ space ~ importParser.rep ~space ~ modelParser.rep).map(x => Module(x._1, x._2, x._3))

  def nameSpaceParser[$: P]: P[NameSpace] = P(LexicalParser.keyword("package") ~/ space ~ LexicalParser.identifier.rep(sep = ".") ~ space).map(NameSpace.apply)

  def importParser[$: P]: P[Import] = P(LexicalParser.keyword("import") ~/ space~ LexicalParser.identifier.rep(sep = ".") ~space).map(Import.apply)

  def modelParser[$:P]: P[Model] = P(LexicalParser.keyword("class") ~/ space~ LexicalParser.identifier ~ ("extends" ~ ExpressionParser.typeRefParser).? ~ (LexicalParser.keyword("with") ~ ExpressionParser.typeRefParser).rep() ~ curlyBraceBlock).map(x => ClassModel(x._1, Seq(), Seq(), x._2, Seq(), x._3, x._4.statements)).log

  def space[$: P]         = P( CharsWhileIn(" \r\n", 0) )

  def newline[$: P]: P[Unit] = P("\n")

  def assignParser[$:P]: P[Assign] = P(LexicalParser.keyword("let") ~ space ~ ("mutable").!.? ~space ~  LexicalParser.identifier ~ space ~ (":" ~/space ~  ExpressionParser.typeRefParser).? ~/ space ~ P(LexicalParser.keyword("=")) ~ block).map(x => Assign(x._2, x._3, x._1.isEmpty, x._4)).log

  def block[$:P]: P[Block] = P(space ~  (curlyBraceBlock | (ExpressionParser.expressionParser ~ space).map(Inline)))

  def commentParser[$:P]: P[_] = P(LexicalParser.comment)

  def exprAsStmt[$:P]: P[Statement] = P( space ~ ExpressionParser.expressionParser ~ space).map(ExprAsStmt)

  def ifStatementParser[$:P]: P[If] = {
    def ifParser[$: P]: P[(Expression, Statement)] = P(LexicalParser.keyword("if") ~ space  ~/  "(" ~/ space ~ ExpressionParser.expressionParser ~ space ~ ")" ~ space ~/ block).map(x => (x._1, x._2))

    def elseParser[$: P]: P[Statement] = P(elifP ~ elseParser.?).map(x => If(x._1, x._2, x._3)) | P(elseP)

    def elifP[$: P]: P[(Expression, Statement)] = P(LexicalParser.keyword("else") ~ LexicalParser.keyword("if") ~/ "(" ~ ExpressionParser.expressionParser ~ ")" ~ block).map(x => (x._1, x._2))

    def elseP[$: P]: P[Statement] = P(LexicalParser.keyword("else") ~/ block).map(x => x)
    P(ifParser ~ elseParser.?).map(x => If(x._1, x._2, x._3)).log
  }

  def fieldParser[$:P]: P[Field] = P(LexicalParser.identifier ~ ":" ~ ExpressionParser.typeRefParser).map(x => Field(x._1, x._2, None))

  def methodParser[$:P]: P[Statement] = P(LexicalParser.keyword("let") ~ space~ ExpressionParser.modifiers  ~ space ~  LexicalParser.identifier ~space ~  "(" ~/ space ~ fieldParser.rep(sep = ",") ~space ~  ")" ~ (":" ~space ~  ExpressionParser.typeRefParser ~space).? ~ space ~ block).map(x => Method(x._2, Seq(), x._3, x._1, x._4, x._5))


  def reassignParser[$:P]: P[Reassign] = P(ExpressionParser.nameParser ~ "<-" ~/ block).map(x => Reassign(x._1, x._2))

//  def statementParser[$:P]: P[Statement] = P(!commentParser ~ (modelParser | ifStatementParser | methodParser | assignParser | reassignParser | exprAsStmt)).log

  def statementParser[$:P]: P[Statement] = P(!commentParser ~ (modelParser | ifStatementParser | methodParser | assignParser | reassignParser | exprAsStmt)).log
  def curlyBraceBlock[$:P]: P[CurlyBraceBlock] =  (space ~ "{" ~/ space~ statementParser.rep(sep = space) ~ space~ "}" ~ space).map(x => CurlyBraceBlock(x)).log

}