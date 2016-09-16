package org.scalahamcrest.oo

import scala.reflect.ClassTag

class Matcher[-A](pred: A => Boolean) extends (A => Boolean) {
  self =>

  def &&[A1 <: A](that: Matcher[A1]) =
    new Matcher[A1](x => self(x) && that(x))

  def ||[A1 <: A](that: Matcher[A1]) =
    new Matcher[A1](x => self(x) || that(x))

  def unary_![A1 <: A] =
    new Matcher[A1](!self(_))

  def apply(x: A): Boolean = pred(x)
}

object Always extends Matcher[Any](_ => true)
object Never  extends Matcher[Any](_ => false)

class EqualTo[-A](expected: A) extends Matcher[A] (
  _ == expected
)

object EqualTo {
  def apply[A](expected: A) = new EqualTo(expected)
}

object Empty extends EqualTo("")
object IsNil extends EqualTo[AnyRef](null)

class InstanceOf[-T : ClassTag] extends Matcher[Any] (
  _ match {
    case _: T => true
    case _    => false
  }
)

object InstanceOf {
  def apply[T : ClassTag] = new InstanceOf[T]
}

class Same[-A <: AnyRef](expected: A) extends Matcher[A] (
  expected eq _
)

object Same {
  def apply[A <: AnyRef](expected: A) = new Same(expected)
}

case class Not[-A](matcher: Matcher[A]) extends Matcher[A] (
  !matcher(_)
)

object Not {
  def apply[A](expected: A): Not[A] = Not(EqualTo(expected))
}

case class Is[-A](matcher: Matcher[A]) extends Matcher[A] (
  matcher(_)
)

object Is {
  def apply[A](expected: A): Is[A] = Is(EqualTo(expected))
}

case class AllOf[-A](matchers: Matcher[A]*) extends Matcher[A] (
  actual => matchers.forall { _(actual) }
)

case class AnyOf[-A](matchers: Matcher[A]*) extends Matcher[A] (
  actual => matchers.exists { _(actual) }
)

class LessThan[-A : Ordering](expected: A) extends Matcher[A] (
  Ordering[A].lt(_, expected)
)

object LessThan {
  def apply[A : Ordering](expected: A) = new LessThan(expected)
}

class GreaterThan[-A : Ordering](expected: A) extends Matcher[A] (
  Ordering[A].gt(_, expected)
)

object GreaterThan {
  def apply[A : Ordering](expected: A) = new GreaterThan(expected)
}

class GreaterThanOrEqualTo[-A : Ordering](expected: A) extends Matcher[A] (
  Ordering[A].gteq(_, expected)
)

object GreaterThanOrEqualTo {
  def apply[A : Ordering](expected: A) = new GreaterThanOrEqualTo(expected)
}

class LessThanOrEqualTo[-A : Ordering](expected: A) extends Matcher[A] (
  Ordering[A].lteq(_, expected)
)

object LessThanOrEqualTo {
  def apply[A : Ordering](expected: A) = new LessThanOrEqualTo(expected)
}

class ComparesEqualTo[-A : Ordering](expected: A) extends Matcher[A] (
  Ordering[A].equiv(_, expected)
)

object ComparesEqualTo {
  def apply[A : Ordering](expected: A) = new ComparesEqualTo(expected)
}

case class Starts(prefix: String) extends Matcher[String] (
  _ startsWith prefix
)

case class Ends(suffix: String) extends Matcher[String] (
  _ endsWith suffix
)

case class Contains(substr: String) extends Matcher[String] (
  _ contains substr
)

case class IgnoringCase(matcher: Matcher[String]) extends Matcher[String] (
  s => matcher(s.toLowerCase)
)

object IgnoringCase {
  def equalTo(str: String)  = IgnoringCase(EqualTo(str.toLowerCase))
  def starts(str: String)   = IgnoringCase(Starts(str.toLowerCase))
  def ends(str: String)     = IgnoringCase(Ends(str.toLowerCase))
  def contains(str: String) = IgnoringCase(Contains(str.toLowerCase))
}

object Blank extends Matcher[String] (
  """\s*""".r.pattern.matcher(_).matches
)

object EmptyOrNil extends AnyOf(IsNil, Empty)
object BlankOrNil extends AnyOf(IsNil, Blank)
