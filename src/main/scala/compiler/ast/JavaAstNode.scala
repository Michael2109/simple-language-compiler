package compiler.ast

object JavaAstNode {

  sealed trait JavaAstNode

  case class CompilationUnit(imports: Seq[ImportDecl], classes: Seq[ClassDecl]) extends JavaAstNode

  case class ImportDecl(packageName: String) extends JavaAstNode

  case class ClassDecl(name: String, fields: Seq[FieldDecl], methods: Seq[MethodDecl]) extends JavaAstNode

  case class FieldDecl(name: String, fieldType: String) extends JavaAstNode

  case class MethodDecl(name: String, returnType: String, fields: Seq[FieldDecl], body: Seq[Statement]) extends JavaAstNode

  case class Parameter(name: String, paramType: String) extends JavaAstNode

  sealed trait Statement extends JavaAstNode

  case class Assignment(variable: String, variableType: String, expression: Expression) extends Statement

  case class IfStatement(condition: Expression, thenBlock: List[Statement], elseBlock: List[Statement]) extends Statement

  sealed trait Expression extends JavaAstNode

  case class Variable(name: String) extends Expression

  case class Literal(value: Any) extends Expression

  case class BinaryOp(left: Expression, operator: String, right: Expression) extends Expression

}