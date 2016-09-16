package org.scalahamcrest.fp

object Asserter {
  def assertThat[A](actual: A, matcher: A => Boolean) =
    assert(matcher(actual))
}
