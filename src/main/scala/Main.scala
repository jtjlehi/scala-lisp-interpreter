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
object Pair:
  def unapply(el: Expression) = el match
    case Cons(hd, Cons(tl, Empty)) => Some(Lyst(hd, tl))
    case _                         => None

object Single:
  def unapply(el: Expression) = el match
    case Cons(el, Empty) => Some(el)
    case _               => None

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
  trait Fn extends RegexParsers:
    def atomParser: Parser[Expression => Either[String, Expression]]
    def apply(atom: Atom)(expr: Expression) = parse(atomParser, atom.str) match
      case Success(result, _) => result(expr)
      case Failure(msg, _)    => Left(msg)
      case Error(msg, _)      => Left(msg)

  def fnPattern(pf: PartialFunction[Expression, Expression], msg: String)(
      el: Expression
  ) = el match
    case pf(m) => Right(m)
    case _     => Left(msg)

  def cons(el: Expression) = el match
    case Pair(hd, tl) => Lyst(hd, tl)
    case el           => Lyst(el)

  object ConsFn extends Fn:
    override def atomParser = "cons".r ^^^ fnPattern(
      { case Pair(hd, tl) => Lyst(hd, tl) },
      "cons requires 2 elements"
    )

  object Chain extends Fn:
    def msg(start: String) = start + " requires a list"
    def a = "a".r ^^^ fnPattern({ case Pair(hd, _) => hd }, msg("car"))
    def d = "d".r ^^^ fnPattern({ case Pair(_, tl) => tl }, msg("cdr"))
    def init(el: Expression): Either[String, Expression] = Right(el)
    // convert c(a|d)*r chain into chain of car and cdr functions
    override def atomParser = phrase("c".r ~> (a | d).* <~ "r".r ^^ {
      _.foldRight(init) { (next, curr) => curr(_) flatMap (next) }
    })

  object AtomFn extends Fn:
    override def atomParser = "atom".r ^^^ fnPattern(
      {
        case Single(Atom(_)) => Atom("t")
        case Single(_)       => Empty
      },
      "atom requires a single argument"
    )

  object EqFn extends Fn:
    override def atomParser = "eq".r ^^^ fnPattern(
      {
        case Pair(Atom(x), Atom(y)) if x == y => Atom("t")
        case Pair(_, _)                       => Empty
      },
      "compare requires exactly 2 params"
    )

  object CondFn extends Fn:
    def cond(expr: Expression): Either[String, Expression] = expr match
      case Cons(Cons(hd, hdtl), tl) =>
        if (Eval(Right(hd)) == Right(Atom("t"))) Right(hdtl) else cond(tl)
      case _ => Right(Empty)
    def atomParser = "cond".r ^^^ cond

  def apply[E](res: Either[E, Expression]) = res map { ??? }

object Print:
  def apply[E](res: Either[E, Expression]) = res match
    case Left(msg)    => println(msg)
    case Right(value) => ???

@main def main =
  val code = "(hello 1.5 'world (this '(is my) () (test)))"
  Print(Eval(Read(code)))
