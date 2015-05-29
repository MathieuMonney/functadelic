package stagedparsec

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait JSONParsersProg extends JSONChallengeParser with Equal {
  import Parser._

  def testValue(in: Rep[Array[Char]]): Rep[Option[(String, String)]] = {
  	phrase(value, StringReader(in))
  }

  def testLine(in: Rep[Array[Char]]): Rep[Option[List[(String, String)]]] = {
    phrase(line, StringReader(in))
  }
  // Bug in the generated code
  def testJson(in: Rep[Array[Char]]): Rep[Option[List[List[(String, String)]]]] = {
    phrase(json, StringReader(in))
  }

  // Generated code doesn't run
  def test_(in: Rep[Array[Char]]): Rep[Option[List[List[Char]]]] = {
    phrase(rep(repsep(chr('a'), chr(','))), StringReader(in))
  }

}

class JSONParserSuite extends FileDiffSuite {
  val prefix = "test-out/"

  def testJsonParsers = {
    withOutFile(prefix + "jsonchallenge-parser") {
      /**
       * Attention: Need to mix in Fat versions of StructOps as well as IfthenElse
       * for optimisations on FatIfs and so on.
       * Note: We are also using our own version of IfThenElseGenFat
       * to generate variables instead of tuples and boundary ends
       * of conditional expressions.
       */
      new JSONParsersProg
          with TokenParsersExp
          with CharParsersExp
          with IfThenElseExpOpt
          with StructOpsFatExpOptCommon
          with MyScalaCompile { self =>

        val codegen = new ScalaGenTokenParsers with ScalaGenFatStructOps with MyScalaGenIfThenElseFat {
          val IR: self.type = self
        }

        codegen.emitSource(testValue _, "testValue", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcValue = compile(testValue)
        scala.Console.println(testcValue("\"hello\":\"you\"".toArray))
        scala.Console.println(testcValue("ello".toArray))
        codegen.reset

        codegen.emitSource(testLine _, "testLine", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcLine = compile(testLine)
        scala.Console.println(testcLine("{\"hello\":\"you\",\"name\":\"Charles Dubois\"}".toArray))
        scala.Console.println(testcLine("{\"name\":\"Charles Dubois\",\"hello\":\"you\"}".toArray))
        scala.Console.println(testcLine("{\"name\":\"Charles Dubois\",\"creditcard\":\"something\"}".toArray))
        scala.Console.println(testcLine("{\"name\":\"Charles Dubois\",\"hello\":\"you\",\"creditcard\":\"something\"}".toArray))
        scala.Console.println(testcLine("{\"name\":\"Charles Dubois\",\"creditcard\":\"something\",\"hello\":\"you\"}".toArray))
        scala.Console.println(testcLine("{\"hello\":\"you\",\"name\":\"Charles Dubois\",\"creditcard\":null}".toArray))
        codegen.reset

        /*codegen.emitSource(testJson _, "json", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcJson = compile(testJson)
        scala.Console.println(testcJson("[{\"name\":\"Charles Dubois\",\"hello\":\"you\",\"creditcard\":\"something\"},\n{\"name\":\"Bill Clinton\",\"hello\":\"you\",\"creditcard\":\"something\"}]".toArray))
        codegen.reset*/

        /*codegen.emitSource(test_ _, "test", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc = compile(test_)
        scala.Console.println(testc("a,a,a a,a,a a,a,a".toArray))
        codegen.reset*/
      }
      assertFileEqualsCheck(prefix + "jsonchallenge-parser")
    }
  }
}