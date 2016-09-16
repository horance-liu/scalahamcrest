package org.scalahamcrest.oo

object Asserter {
  def assertThat[A](actual: A, matcher: Matcher[A]) =
    assert(matcher(actual))
}