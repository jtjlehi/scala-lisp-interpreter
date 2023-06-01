import scala.annotation.tailrec
import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait Expression
case class Atom(str: String) extends Expression
case class Number(num: Float) extends Expression
object Number:
  def apply(str: String): Number = Number(str.toFloat)
case class Problem(msg: String) extends Expression

trait Lyst extends Expression
case object Empty extends Lyst
case class Cons(hd: Expression, tl: Lyst) extends Lyst
object Cons:
  def apply(el: Atom | Number): Cons = Cons(el, Empty)
  def apply(el: Lyst) = el
  def apply(str: String): Cons = Cons(Atom(str))
  def apply(num: Float): Cons = Cons(Number(num))

  def quote(el: Expression) = el match
    case el: Lyst     => Cons(Atom("quote"), el)
    case el: Atom     => Cons(Atom("quote"), Cons(el))
    case el: Number   => Cons(Atom("quote"), Cons(el))
    case err: Problem => err

object Read extends RegexParsers:
  // basic tokens
  private def atom = ("""[\S&&[^\(\)']]+""".r ^^ { Atom(_) }).named("atom")
  private def number = ("""(\d)+(\.\d+)?""".r ^^ { Number(_) }).named("number")
  private def lParen = """\(""".r.named("left paren")
  private def rParen = """\)""".r.named("right paren")
  private def quote = "'".r.named("quote mark")

  // combined tokens
  private def primative = atom | number
  private def emptyLyst = lParen ~ rParen ^^^ Empty
  // a list can contain lists, quoted primatives, and quoted lists
  private def basicList: Parser[Expression] =
    (lParen ~> read.* <~ rParen ^^ { _.foldRight(Empty)(Cons.apply) })
  // quoted list or primative
  private def quoteEl = quote ~> read ^^ { Cons.quote(_) }

  def read: Parser[Expression] = primative | quoteEl | basicList | emptyLyst

  def apply(code: String) = parse(phrase(read), code)

@main def main =
  val code = "(hello 1.5 'world (this '(is my) () (test)))"
  println(Read(code))
