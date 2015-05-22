package stagedparsec

import scala.virtualization.lms.common._
import lms._
import lms.util._

/**
 * An implementation of staged parser combinators
 * based on a previous implementation in
 * https://github.com/manojo/experiments/
 */

trait StagedParsers
    extends ParseResultOps
    with OptionOps
    with ReaderOps
    with MyTupleOps
    with IfThenElse
    with Functions
    with LiftVariables
    with While
    with ListOps {


  case class SeqParser[T: Manifest, U: Manifest](p: Parser[T], q: Parser[U]) extends Parser[(T, U)] {
    def proj(i: Int) = ???
    def apply(in: Rep[Input]) = (for (l <- p; r <- q) yield make_tuple2(l, r)).apply(in)
  }

  /**
   * Case class for the repetition of a parser
   * Used for optimization: filter results on the fly
   */
  case class RepParser[T: Manifest](p: Parser[T]) extends Parser[List[T]] {
    def apply(in: Rep[Input]) = repFold(p)(List[T]().asInstanceOf[Rep[List[T]]], { (ls: Rep[List[T]], t: Rep[T]) => ls ++ List(t) }).apply(in)
    def filter(f: Rep[T] => Rep[Boolean]): Parser[List[T]] = repFold(p)(List[T]().asInstanceOf[Rep[List[T]]], (x: Rep[List[T]], y: Rep[T]) => if(f(y)) x ++ List(y) else x)
  }

  /**
   * Case class for interleaved repetitions
   * Also used for optimization
   */
  case class RepSepParser[T: Manifest, U: Manifest](p: Parser[T], q: Parser[U]) extends Parser[List[T]] {
    def apply(in: Rep[Input]) = ((p ~ rep(q ~> p)) ^^ { x => x._1 :: x._2 } | success(List[T]())).apply(in)
    def filter(f: Rep[T] => Rep[Boolean]): Parser[List[T]] = (p ~ rep(q ~> p).filter(f)) ^^ { x => if(f(x._1)) x._1 :: x._2 else x._2 }// | success(List[T]())
  }

  abstract class Parser[T: Manifest]
      extends (Rep[Input] => Rep[ParseResult[T]]) {

    /**
     * The flatMap operation
     */
    def flatMap[U: Manifest](f: Rep[T] => Parser[U]) = Parser[U] { input =>
      val tmp = this(input)
      if (tmp.isEmpty) Failure[U](input)
      else {
        val x = f(tmp.get).apply(tmp.next)
        x //if (x.isEmpty) Failure[U](input) else x
      }
    }

    def >>[U: Manifest](f: Rep[T] => Parser[U]) = flatMap(f)

    /**
     * The concat operation
     */
    def ~[U: Manifest](that: Parser[U]): Parser[(T, U)] = SeqParser(this, that)

    /**
     * get right hand side result
     */
    def ~>[U: Manifest](that: => Parser[U]): Parser[U] =
      this flatMap { l => that }

    /**
     * get left hand side result
     */
    def <~[U: Manifest](that: => Parser[U]): Parser[T] =
      for (l <- this; r <- that) yield l

    /**
     * The map operation
     */
    def map[U: Manifest](f: Rep[T] => Rep[U]) = Parser[U] { input =>
      this(input) map f
    }

    /**
     * The alternat operation
     */
    def |[U >: T: Manifest](that: Parser[U]) = {
      val p = Parser[U]{ in =>
        val tmpSelf = topLevel(this)
        val x = tmpSelf(in)
        if(x.isEmpty) that(in) else x.asInstanceOf[Rep[ParseResult[U]]]
      }
      topLevel(p)
    }

    def ^^[U: Manifest](f: Rep[T] => Rep[U]) = this.map(f)
    def ^^^[U: Manifest](u: Rep[U]) = this.map(x => u)
  }

  def topLevel[T: Manifest](p: Parser[T]): Parser[T] = {
    val f = doLambda { p }
    Parser[T] { i => f(i) }
  }

  def repFold[T: Manifest, U: Manifest](p: => Parser[T])(z: Rep[U], f: (Rep[U], Rep[T]) => Rep[U]) = Parser[U] { in =>

      var s = Success[U](z, in)

      var old = unit(-1)
      var continue = unit(true)
      var curInput = in

      while (readVar(continue) && readVar(old) != readVar(curInput).offset) {
        old = readVar(curInput).offset
        val tmp = p(curInput)

        if (tmp.isEmpty) { continue = unit(false) }
        else {
          s = Success(f(readVar(s).get, tmp.get), tmp.next)
          curInput = tmp.next
        }
      }
      readVar(s)
    }

    def rep[T: Manifest](p: => Parser[T]) = RepParser(p)

    def repSep[T: Manifest, U: Manifest](p: => Parser[T], q: => Parser[U]) = RepSepParser(p, q)

    def opt[T: Manifest](p: Parser[T]) = Parser[Option[T]] { in =>
      val x = p(in)
      if (x.isEmpty) Success(none[T](), x.next)
      else Success(Some(x.get), x.next)
   }

    def success[T:Manifest](v:Rep[T]) = new Parser[T] {
      def apply(in: Rep[Input]) = Success(v, in)
    }

  /**
   * a 'conditional' parser
   * lifts conditional expressions to parser level
   */
  def __ifThenElse[A: Manifest](
    cond: Rep[Boolean],
    thenp: => Parser[A],
    elsep: => Parser[A]
  ): Parser[A] = Parser[A] { input => if (cond) thenp(input) else elsep(input) }

  /**
   * companion object for apply function
   */
  object Parser {
    def apply[T: Manifest](f: Rep[Input] => Rep[ParseResult[T]]) = new Parser[T] {
      def apply(in: Rep[Input]) = f(in)
    }

    /**
     * run a parser, and return an `Option`
     */
    def phrase[T: Manifest](p: => Parser[T], in: Rep[Input]): Rep[Option[T]] = {
      val presult = p(in)
      val res = if (presult.isEmpty) none[T]() else Some(presult.get)
      res
    }
  }
}

trait StagedParsersExp
    extends StagedParsers
    with ParseResultOpsExp
    with OptionOpsExp
    with MyTupleOpsExp
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with EqualExpOpt
    with FunctionsExp
    with VariablesExp
    with WhileExp
    with ListOpsExp


trait ScalaGenStagedParsers
    extends ScalaGenParseResultOps
    with ScalaGenOptionOps
    with ScalaGenMyTupleOps
    with ScalaGenIfThenElse
    with ScalaGenBooleanOps
    with ScalaGenEqual
    with ScalaGenFunctions 
    with ScalaGenVariables
    with ScalaGenWhile
    with ScalaGenListOps {
  val IR: StagedParsersExp
}
