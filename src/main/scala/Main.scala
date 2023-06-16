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

// Lyst constructors
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

// Lyst partial destructors
object Pair:
  def unapply(el: Expression) = el match
    case Cons(hd, Cons(tl, Empty)) => Some(Lyst(hd, tl))
    case _                         => None
object Single:
  def unapply(el: Expression) = el match
    case Cons(el, Empty) => Some(el)
    case _               => None
def sublist: PartialFunction[Expression, (Expression, Lyst, Lyst)] = {
  case Cons(Cons(a, b), tl) =>
    (a, b, tl)
}
def subsublist: PartialFunction[Expression, (String, Lyst, Lyst, Lyst)] = {
  case sublist(Cons(Atom(str), innerTl), tl, a) => (str, innerTl, tl, a)
}

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
  type ExprRes = Either[String, Expression]
  trait Fn extends RegexParsers:
    def atomParser: Parser[Expression => ExprRes]
    def apply(atom: Atom)(expr: Expression) = parse(atomParser, atom.str) match
      case Success(result, _) => result(expr)
      case Failure(msg, _)    => Left(msg)
      case Error(msg, _)      => Left(msg)

  def fnPattern(pf: PartialFunction[Expression, Expression], msg: String)(
      el: Expression
  ) = el match
    case pf(m) => Right(m)
    case _     => Left(msg)

  def cons(el: Expression) =
    el match
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
    def init(el: Expression): ExprRes = Right(el)
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

  def assoc(a: Lyst, x: String): ExprRes = a match
    case sublist(Atom(k), v, _) if k == x => Right(v)
    case sublist(_, _, tl)                => assoc(tl, x)
    case _                                => Left("element not found in list")

  object FnPattern extends RegexParsers:
    type PartExpr[T] = PartialFunction[(Expression, Lyst), T]
    def partFn(p: PartExpr[ExprRes])(e: Expression, a: Lyst) =
      (e, a) match
        case p(o) => o
        case _    => Left("invalid params")

    def single: PartExpr[(Expression, Lyst)] = { case (Cons(hd, _), a) =>
      (hd, a)
    }
    def evalHd(fn: Expression => Either[String, Expression]) = partFn {
      case single(hd, a) => Eval(Cons(hd, a)) flatMap fn
    }

    def quote = "quote".r ^^^ partFn { case single(hd, _) => Right(hd) }
    def atom = "atom".r ^^^ evalHd {
      _ match
        case Atom(_) => Right(Atom("t"))
        case _       => Right(Empty)
    }
    def number = "number".r ^^^ evalHd {
      _ match
        case Number(_) => Right(Atom("t"))
        case _         => Right(Empty)
    }
    def eqFn = "eq".r ^^^ partFn { case (Cons(x, Cons(y, _)), a) =>
      for
        x <- Eval(Cons(x, a))
        y <- Eval(Cons(y, a))
      yield if (x == y) Atom("t") else Empty
    }
    def cond =
      // evcon defined to allow for recursion
      def evcon: (Expression, Lyst) => ExprRes = partFn {
        case (sublist(hd, hdtl, tl), a) =>
          for
            x <- Eval(Cons(hd, a))
            res <- if (x == Atom("t")) Eval(Cons(hdtl, a)) else evcon(tl, a)
          yield res
      }
      "cond".r ^^^ evcon
    def car = "car".r ^^^ evalHd {
      _ match
        case Cons(hd, _) => Right(hd)
        case _           => Left("invalid params")
    }
    def cdr = "cdr".r ^^^ evalHd {
      _ match
        case Cons(_, tl) => Right(tl)
        case _           => Left("invalid params")
    }
    def cons = "cons".r ^^^ partFn { case (Cons(hd, hdtl), a) =>
      for
        hd <- Eval(Cons(hd, a))
        tl <- Eval(Cons(hdtl, a))
      yield Lyst(hd, tl)
    }
    def default = ".*".r ^^ { str => (tl: Expression, a: Lyst) =>
      val x = Atom(str)
      def assoc(y: Lyst): Either[String, Lyst] = y match
        case Cons(Cons(hdhd, hdtl), tl) =>
          if (hdhd == x) Right(hdtl) else assoc(y)
        case _ => Left("assoc list is invalid")
      def evlis(m: Expression): Either[String, Lyst] = m match
        case Empty => Right(Empty)
        case Cons(hd, tl) =>
          for
            hd <- Eval(Cons(hd, a))
            tl <- evlis(tl)
          yield Cons(hd, tl)
        case _ => Left("invald params")
      for
        hd <- assoc(a)
        tl <- evlis(tl)
        res <- Eval(Cons(Cons(hd, tl), a))
      yield res
    }
    def parser = quote | atom | number | eqFn | cond | car | default
    def apply(e: String, tl: Expression, a: Lyst): ExprRes =
      parse(parser, e) match
        case Success(result, _) => result(tl, a)
        case Failure(msg, _)    => Left(msg)
        case Error(msg, _)      => Left(msg)

  def apply(res: Expression): ExprRes = apply(Right(res))
  def apply(res: ExprRes): ExprRes = res flatMap {
    _ match
      case Cons(n @ Number(_), _)               => Right(n)
      case Cons(Atom(x), a)                     => assoc(a, x)
      case sublist(Atom(e), tl, a)              => FnPattern(e, tl, a)
      case subsublist("label", innerTl, tl, a)  => ???
      case subsublist("lambda", innerTl, tl, a) => ???
      case _                                    => Right(Empty)
  }

object Print:
  def apply(res: Either[String, Expression]) = res match
    case Left(msg)    => println(msg)
    case Right(value) => ???

@main def main =
  val code = "(hello 1.5 'world (this '(is my) () (test)))"
  Print(Eval(Read(code)))
  /*
// is it an atom
atom [e] -> assoc [e; a]
// is it a list with first element an atom
atom [car [e]] → [
  eq [car [e]; QUOTE] → cadr [e];
  eq [car [e]; ATOM] → atom [eval [cadr [e]; a]];
  eq [car [e]; EQ] → [eval [cadr [e]; a] = eval [caddr [e]; a]];
  eq [car [e]; COND] → evcon [cdr [e]; a];
  eq [car [e]; CAR] → car [eval [cadr [e]; a]];
  eq [car [e]; CDR] → cdr [eval [cadr [e]; a]];
  eq [car [e]; CONS] → cons [eval [cadr [e]; a]; eval [caddr [e]; a]];
  // run function from a
  T → eval [cons [
      assoc [car [e]; a];
      evlis [cdr [e]; a]];
    a]];
// is it a list with the first element a list?
// LABELS
eq [caar [e]; LABEL] → eval [
  cons [caddar [e]; cdr [e]];
  cons [list [cadar [e]; car [e]; a]]];
// LAMBDAS
eq [caar [e]; LAMBDA] → eval [
  caddar [e];
  append [pair [cadar [e]; evlis [cdr [e]; a]; a]]]
   */
