import scala.annotation.tailrec
import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait ReadError
case class LexError(msg: String) extends ReadError
case class ParseError(msg: String) extends ReadError

// type L = Expression.Lyst | Expression.Empty
// enum Expression:
//   case Atom(str: String)
//   case Number(num: Float)
//   case Empty()
//   // a list comes from a quoted list
//   case Lyst(expressions: List[Expression])
//   case Quote(value: Expression)
//   case EvalError(msg: String)

sealed trait Expression
case class Atom(str: String) extends Expression

object Read extends RegexParsers:
  import Expression.*

  def atom = ("""[\S&&[^\(\)']]+""".r ^^ { Atom(_) }).named("atom")
  def number =
    ("""(\d)+(\.\d+)?""".r ^^ { i => Number(i.toFloat) }).named("number")
  def lParen = """\(""".r
  def rParen = """\)""".r
  def quote = "'".r

  private def primative = atom | number

  private def rightParen = accept(
    "right paren",
    { case rParen => EvalError("unexpected right paren") }
  )

  private def emptyLyst = lParen ~ rParen ^^^ Empty()
  // a list can contain lists, quoted primatives, and quoted lists
  private def basicList: Parser[Expression] =
    lParen ~> (basicList | primative | quoteEl).* <~ rParen ^^ { Lyst(_) }

  // quoted list or primative
  private def quoteEl = quote ~> (basicList | primative) ^^ { Quote(_) }

  def read: Parser[Expression] = primative |
    rightParen |
    quoteEl |
    emptyLyst |
    basicList |
    success(Empty()).named("empty eval")

  def apply(in: Input) = read(in)

@main def main =
  val code = "(hello 1.5 'world (this '(is my) () (test)))"
