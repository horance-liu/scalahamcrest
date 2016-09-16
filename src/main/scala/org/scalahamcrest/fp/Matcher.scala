package org.scalahamcrest.fp

import scala.reflect.ClassTag

object Matcher {
  class Matcher[-A](pred: A => Boolean) extends (A => Boolean) {
    self =>

    def &&[A1 <: A](that: A1 => Boolean): A1 => Boolean =
      x => self(x) && that(x)

    def ||[A1 <: A](that: A1 => Boolean): A1 => Boolean =
      x => self(x) || that(x)

    def unary_![A1 <: A]: A1 => Boolean =
      !self(_)

    def apply(x: A): Boolean = pred(x)
  }

  import languageFeature.implicitConversions
  implicit def toMatcher[A](pred: A => Boolean) = new Matcher(pred)

  def equalTo[T](expected: T): T => Boolean =
    expected == _

  def is[T](matcher: T => Boolean): T => Boolean =
    matcher

  def is[T](expected: T): T => Boolean = is(equalTo(expected))

  def not[T](matcher: T => Boolean): T => Boolean =
    !matcher(_)

  def not[T](expected: T): T => Boolean = not(equalTo(expected))

  def always: Any => Boolean = _ => true
  def never:  Any => Boolean = _ => false

  def nil = equalTo[AnyRef](null)
  def same[T <: AnyRef](t: T): T => Boolean = t eq _

  def instanceOf[T : ClassTag]: Any => Boolean = x =>
    x match {
      case _: T => true
      case _    => false
    }

  def allOf[T](matchers: (T => Boolean)*): T => Boolean =
    actual => matchers.forall(_(actual))

  def anyOf[T](matchers: (T => Boolean)*): T => Boolean =
    actual => matchers.exists(_(actual))

  type DoubleMatcher = Double => Boolean

  def nan: DoubleMatcher =
    _.isNaN

  def closeTo(value: Double, delta: Double): DoubleMatcher =
    n => (n - value).abs - delta <= 0

  def gt[T : Ordering](expected: T): T => Boolean =
    Ordering[T].gt(_, expected)

  def lt[T : Ordering](expected: T): T => Boolean =
    Ordering[T].lt(_, expected)

  def gteq[T : Ordering](expected: T): T => Boolean =
    Ordering[T].gteq(_, expected)

  def lteq[T : Ordering](expected: T): T => Boolean =
    Ordering[T].lteq(_, expected)

  def equiv[T : Ordering](expected: T): T => Boolean =
    Ordering[T].equiv(_, expected)

  type StringMatcher = String => String => Boolean

  def ignoring_case(matcher: StringMatcher): StringMatcher = substr =>
    str => matcher(substr.toLowerCase)(str.toLowerCase)

  def starts: StringMatcher = prefix =>
    _ startsWith prefix

  def ends: StringMatcher = suffix =>
    _ endsWith suffix

  def contains: StringMatcher = substr =>
    _ contains substr

  def empty = equalTo("")
  def blank: String => Boolean ="""\s*""".r.pattern.matcher(_).matches

  def empty_or_nil = anyOf(nil, equalTo(""))
  def blank_or_nil = anyOf(nil, blank)
}
