package stagedparsec

import lms._
import lms.util._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects


trait TokenParsers extends StagedParsers with CharParsers with StringStructOps with BarrierOps {

  def processIdent(s: Rep[String]) =
    if (s == unit("true") || s == unit("null") || s == unit("false")) unit("Keyword(") + s + unit(")")
    else unit("NoToken")

  def keyword: Parser[String] = letter ~ repToS(letter /*| digit(in)*/ ) ^^ {
    x: Rep[(Char, String)] => processIdent(x._1 + x._2)
  }

  def numeric: Parser[String] = digit ~ repToS(digit) ^^ {
    x: Rep[(Char, String)] => (x._1 + x._2)
  }

  def wholeNumber: Parser[Int] =
    digit2Int >> { x: Rep[Int] =>
      repFold(digit2Int)(x, (res: Rep[Int], y: Rep[Int]) => (res * unit(10) + y))
    }

  def intLit: Parser[Int] =
    opt(chr('-')) ~ wholeNumber ^^ { x =>
      if (x._1.isDefined) unit(-1) * x._2 else x._2
    }

  // the syntax for parsing double is too lousy, everything becomes double
  def chr(c: Char) = accept(unit(c))

  def stringLit: Parser[String] = (
    accept(unit('"')) ~>
    repToS(acceptIf((x: Rep[Char]) => x != unit('"')))
    <~ accept(unit('"'))
  )

  /*
  def stringLit(in: Rep[Input]): Parser[String] =
    chr(in, '"') ~> str(in, _ != unit('"') ,true) <~ chr(in,'"') ^^ { _.mkString }
  */

  def whitespaces: Parser[String] =
    repToS(acceptIf { x: Rep[Char] => x == unit(' ') || x == unit('\n') }) ^^^ { unit("") }

  def repToS(p: Parser[Char]): Parser[String] =
    repFold(p)(unit(""), (res: Rep[String], x: Rep[Char]) => res + x)

  def repToS_f(p: Parser[Char]): Parser[String] = {
    repFold(p)(unit(""), (res: Rep[String], x: Rep[Char]) => res)
  }

  //TODO: ensure that we have an Idx parser here, and not any
  //Int parser
  //  def repToSStruct(p: Parser[Int]) : Parser[StringStruct] =
  //    repFold(p)(String(, (res: Rep[String], x: Rep[Char]) => res + x)

  def word: Parser[String] = letter ~ repToS(letter) ^^ { x => x._1 + x._2 }

  /*def token: Parser[Token] =
    ( identChar ~ rep( identChar | digit ) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | digit ~ rep( digit ) ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
    | '\'' ~ rep( chrExcept('\'', '\n', EofCh) ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "") }
    | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    | EofCh ^^^ EOF
    | '\'' ~> failure ^^^ NoToken
    | '\"' ~> failure ^^^ NoToken
    | delim
    | failure("illegal character")
    )
  def word : Parser[String] =
  def stringLit
  */

  def accept(cs: List[Rep[Char]]): Parser[String] = cs match {
    case Nil => Parser { i => Success(unit(""), i) }
    case x :: xs => accept(x) ~ accept(xs) ^^ {
      y: Rep[(Char, String)] => y._1 + y._2
    }
  }

  def accept(s: String): Parser[String] =
    accept(s.toList.map { c => unit(c) })

  //we can also accept a string and return a boolean value. We only care that
  //the string can be parsed
  // TODO: LMS should understand this pattern and replace calls to p ^^^ f
  // with p_bool ^^^ f
  /*def acceptB(s: String): Parser[Boolean] = {
    val len = staticData(s.length)
    val arr = staticData(s.toArray)
    Parser[Boolean] { in =>

      if (in.offset + len > in.input.length) Failure[Boolean](in)
      else {
        barrierSync("TODO: Hack!")
        var count = unit(0); var matches = unit(true)
        while (matches && count < len) {
          if (in.input(in.offset + readVar(count)) != arr(readVar(count))) matches = unit(false)
          count = readVar(count) + unit(1)
        }

        if (matches) Success(unit(true), StringReader(in.input, in.offset + len))
        else Failure[Boolean](in)
      }

    }
  }*/

  def doubleLit: Parser[Double] = (
    ((opt(chr('-')) ~ numeric) ^^ {
      x: Rep[(Option[Char], String)] =>
        if (x._1.isDefined) x._1.get + x._2 else x._2
    })
    ~ (chr('.') ~> numeric)
  ) ^^ { x => (x._1 + unit(".") + x._2).toDouble }

  /* specialized rep for retrieving a stringstruct
   * cannot be implemented using a rep because the zero element
   * does not have any notion of the start position
   */

  def stringStruct(p: Parser[Int]) = Parser[StringStruct] { in =>
    //println(unit("enter stringStruct {"))
    barrierSync("TODO: Hack!")
    var old = unit(-1)
    var continue = unit(true)
    var curInput = in

    while (readVar(continue) && readVar(old) != readVar(curInput).offset) {
      old = readVar(curInput).offset
      val x = p(curInput)
      if (x.isEmpty) continue = unit(false)
      else curInput = x.next
    }

    // println(unit("} exit stringStruct: '") + String(in, pos, cur - pos).mkString +
    //  unit(" = ") + String(in, pos, readVar(s).get).mkString + unit("'"))
    Success[StringStruct](String(in.input, in.offset, readVar(curInput).offset - in.offset), curInput)
  }

}

trait TokenParsersExp extends TokenParsers with StagedParsersExp with CharParsersExp with StringStructOpsExp with BarrierOpsExp

trait ScalaGenTokenParsers extends ScalaGenStagedParsers with ScalaGenCharParsers with ScalaGenStringStructOps with ScalaGenBarrierOps {
  val IR: TokenParsersExp
}