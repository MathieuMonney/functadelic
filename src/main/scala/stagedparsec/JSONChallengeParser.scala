package stagedparsec

import lms._
import lms.util._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

trait JSONChallengeParser extends TokenParsers {
	
    def value = (stringLit <~ chr(':')) ~ (stringLit | keyword)

    def line = (chr('{') ~> repsep(value, chr(',')).filter( x => x._1 == "name" || x._1 == "creditcard") <~ chr('}'))

    // TODO: filter
    def json = chr('[') ~> repsep(line, chr(',')) <~ chr(']')
}