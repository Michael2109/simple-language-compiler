package compiler.ast

import compiler.ast.Ast
import compiler.ast.Ast.{Assign, Block, ClassModel, CurlyBraceBlock, ExprAsStmt, Expression, Field, Import, Inline, IntConst, Method, Model, RefLocal, Statement}

import scala.annotation.tailrec
object AstToJavaAst {

  def toJava(module: Ast.Module): JavaAstNode.CompilationUnit = {
    val imports = module.imports.map(toJava)
    val classes = module.models.map(toJava)
      JavaAstNode.CompilationUnit(imports, classes)
  }

  private def toJava(importD: Import): JavaAstNode.ImportDecl = {
    JavaAstNode.ImportDecl(importD.loc.mkString("."))
  }

  private def toJava(model: Model): JavaAstNode.ClassDecl = model match
    case c: ClassModel => toJava(c)
    case _ => throw new Exception()

  private def toJava(classModel: ClassModel): JavaAstNode.ClassDecl = {
    val fields = classModel.fields.map(toJava)
    val methods = classModel.statements.map(toJava).filter(x => x.isInstanceOf[Method]).map(x => x.asInstanceOf[Method]).map(toJava)



    JavaAstNode.ClassDecl(classModel.name, fields, methods)
  }

  private def toJava(field: Field): JavaAstNode.FieldDecl = {
    val refLocalName = field.`type`.ref.asInstanceOf[RefLocal].name
    JavaAstNode.FieldDecl(field.name, refLocalName)
  }

  private def toJava(method: Method): JavaAstNode.MethodDecl = {
    val methodType: String = method.returnType.get.ref.asInstanceOf[RefLocal].name;
    val fields = method.fields.map(toJava)
    val statements = toJava(method.body)
    JavaAstNode.MethodDecl(method.name, methodType,fields, statements)
  }

  private def toJava(statement: Statement): JavaAstNode.Statement = {

    statement match {
      case assignment: Assign => assignmentToJava(assignment)
    }
  }

  private def assignmentToJava(assign: Assign): JavaAstNode.Assignment = {

    val statements = toJava(assign.block)
    val expression =toJava(statements.head.asInstanceOf[ExprAsStmt].expression)

    JavaAstNode.Assignment(assign.name, assign.`type`.get.ref.asInstanceOf[RefLocal].name,expression)
  }

  @tailrec
  private def toJava(expression: Ast.Expression): JavaAstNode.Expression = expression match {
    case intConst: IntConst => toJava(intConst)
  }

  private def toJava(block: Block): Seq[JavaAstNode.Statement] = block match {
    case inline: Inline => Seq(toJava(ExprAsStmt(inline.expression)))
    case curlyBraceBlock: CurlyBraceBlock => curlyBraceBlock.statements.map(toJava)
  }
}
