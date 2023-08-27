package compiler.compiler

import compiler.ast.Ast
import compiler.code_gen.CodeGen
import compiler.parser.StatementParser
import fastparse.Parsed

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.file.{Path, Paths}
import scala.io.Source
import fastparse._
object Compiler {

  def compile(commandLineOptions: Map[CommandLineOption, Any], classPath: Path, pathsToCompile: List[Path], outputDir: Path)= {

    val absolutePathsToCompile: List[Path] = pathsToCompile.map(path => Paths.get(classPath.toString, path.toString))

    val cobaltFiles: List[String] = absolutePathsToCompile.map(x => Source.fromFile(x.toFile).mkString)

    // Parse them
    val asts = cobaltFiles.map(cobaltFile => parse(cobaltFile, StatementParser.moduleParser(_)))

    val modules: Seq[Ast.Module] = asts.map {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed compiling: " + a + " : " + b + " : " + c + " : " + c.stack)
    }
/*
    // Process AST
    val modelIRs: Seq[ModelIR] = modules.map(x => AST2IR.astToIR(x)).head

    // Generate code
    val moduleBytecodes: Seq[Array[Byte]] = modelIRs.map(CodeGen.genModelCode)

    // Save to destination directory
    val generatedFilePath = outputDir.resolve(pathsToCompile(0))

    val filePath = Paths.get(generatedFilePath.toString.replaceFirst("[.][^.]+$", "") + ".class")

    // Create class file
    val file = filePath.toFile
    file.createNewFile()

    val bos = new BufferedOutputStream(new FileOutputStream(filePath.toFile))

    bos.write(moduleBytecodes(0))
    bos.close()*/
  }
}
