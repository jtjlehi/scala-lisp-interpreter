import scala.annotation.tailrec
import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait Expression
case class Atom(str: String) extends Expression
case class Number(num: Float) extends Expression
object Number:
  def apply(str: String): Number = Number(str.toFloat)

trait Lyst extends Expression
case object Empty extends Lyst
case class Cons(hd: Expression, tl: Lyst) extends Lyst
object Lyst:
  def apply() = Empty
  def apply(el: Expression): Lyst = el match
    case l: Lyst    => l
    case el: Number => Cons(el, Empty)
    case el: Atom   => Cons(el, Empty)
  def apply(hd: Expression, tl: Expression): Cons = Cons(hd, Lyst(tl))
  def apply(str: String): Lyst = Lyst(Atom(str))
  def apply(num: Float): Lyst = Lyst(Number(num))

  def quote(el: Expression) = el match
    case el: Lyst   => Cons(Atom("quote"), el)
    case el: Atom   => Cons(Atom("quote"), Lyst(el))
    case el: Number => Cons(Atom("quote"), Lyst(el))

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
  private def basicList =
    (lParen ~> read.* <~ rParen ^^ { _.foldRight(Empty)(Cons.apply) })
  // quoted list or primative
  private def quoteEl = quote ~> read ^^ { Lyst.quote(_) }

  def read: Parser[Expression] = primative | quoteEl | basicList | emptyLyst
  def apply(code: String) = parse(phrase(read), code) match
    case Success(result, next) => Right(result)
    case Failure(msg, next)    => Left(msg)
    case Error(msg, next)      => Left(msg)

object Eval:
  def cons(el: Expression) = el match
    case Cons(hd, tl) => Lyst(hd, tl)
    case el           => Lyst(el)

  def car(el: Expression) = el match
    case Cons(hd, _) => Right(hd)
    case _           => Left("car requires a list")
  def cdr(el: Expression) = el match
    case Cons(_, tl) => Right(tl)
    case _           => Left("cdr requires a list")
  object Chain extends RegexParsers:
    def a = "a".r ^^^ car
    def d = "d".r ^^^ cdr
    def init(el: Expression): Either[String, Expression] = Right(el)
    def parser = "c".r ~> (a | d).* <~ "r".r ^^ { l =>
      l.foldRight(init)((next, curr) => curr(_).flatMap(next))
    }
    def apply(in: String) = parse(parser, in)

  def apply[E](res: Either[E, Expression]) = res map { ??? }

object Print:
  def apply[E](res: Either[E, Expression]) = res match
    case Left(msg)    => println(msg)
    case Right(value) => ???

@main def main =
  val code = "(hello 1.5 'world (this '(is my) () (test)))"
  Print(Eval(Read(code)))
