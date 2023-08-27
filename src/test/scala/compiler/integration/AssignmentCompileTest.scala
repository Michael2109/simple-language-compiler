package compiler.integration

import compiler.utils.CompilerUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.nio.file.Paths

class AssignmentCompileTest extends AnyFlatSpec with should.Matchers {

  behavior of "Assignment parser"

  it should "compile" in {


      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/AssignmentTest"))
      output(0) shouldBe "10"

  }

}
