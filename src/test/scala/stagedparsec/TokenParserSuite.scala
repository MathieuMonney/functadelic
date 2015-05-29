package stagedparsec

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait TokenParsersProg extends TokenParsers with Equal {
  import Parser._

  def word_(in: Rep[Array[Char]]) : Rep[Option[String]] = {
  	phrase(word, StringReader(in))
  }

  def keyword_(in: Rep[Array[Char]]) : Rep[Option[String]] = {
    phrase(keyword, StringReader(in))
  }

  // Should that really return NoToken?
  def keywordOr(in: Rep[Array[Char]]): Rep[Option[String]] = {
    val parser = keyword | word
    phrase(parser, StringReader(in))
  }

}

class TokenParserSuite extends FileDiffSuite {
  val prefix = "test-out/"

  def testTokenParsers = {
    withOutFile(prefix + "token-parser") {
      
      new TokenParsersProg
          with TokenParsersExp
          with IfThenElseExpOpt
          with StructOpsFatExpOptCommon
          with MyScalaCompile { self =>

        val codegen = new ScalaGenTokenParsers with ScalaGenFatStructOps with MyScalaGenIfThenElseFat {
          val IR: self.type = self
        }

        codegen.emitSource(word_ _, "word", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcWord = compile(word_)
        scala.Console.println(testcWord("hello".toArray))
        scala.Console.println(testcWord("ello-".toArray))
        scala.Console.println(testcWord("23ello-".toArray))
        codegen.reset

        codegen.emitSource(keyword_ _, "keyword", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcKeyword = compile(keyword_)
        scala.Console.println(testcKeyword("hello".toArray))
        scala.Console.println(testcKeyword("null".toArray))
        scala.Console.println(testcKeyword("true".toArray))
        scala.Console.println(testcKeyword("true23".toArray))
        scala.Console.println(testcKeyword("32true".toArray))
        codegen.reset

        codegen.emitSource(keywordOr _, "keywordOr", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcKeywordOr = compile(keywordOr)
        scala.Console.println(testcKeywordOr("hello".toArray))
        scala.Console.println(testcKeywordOr("null".toArray))
        scala.Console.println(testcKeywordOr("truec".toArray))
        codegen.reset
      }
      assertFileEqualsCheck(prefix + "token-parser")
    }
  }
}
