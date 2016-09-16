package org.scalahamcrest.fp

import org.scalatest.{FunSpec, Matchers}

import Matcher._
import Asserter._

class MatcherSpec extends FunSpec {
  describe("EqualTo") {
    assertThat(2, equalTo(2))
    assertThat(2, is(2))
  }

  describe("Not EqualTo") {
    assertThat(1, not(equalTo(2)))
    assertThat(1, !equalTo(2))
    assertThat(1, not(2))
  }

  describe("InstanceOf") {
    assertThat(2, instanceOf[Int])
    assertThat(2, not(instanceOf[String]))
    assertThat(2, !instanceOf[String])

    assertThat("string", instanceOf[String])
    assertThat("string", !instanceOf[Int])

    assertThat(Nil, instanceOf[List[_]])
    assertThat(None, instanceOf[Option[_]])
  }

  describe("Same") {
    assertThat("horance", same("horance"))
  }

  describe("AnyOf") {
    assertThat(2, anyOf(never, !equalTo(2), equalTo(2)))
    assertThat(2, never || !equalTo(2) || equalTo(2))
  }

  describe("AllOf") {
    assertThat(2, allOf(always, instanceOf[Int], is(2), equalTo(2)))
    assertThat(2, always && instanceOf[Int] && is(2) && equalTo(2))
  }

  describe("StringOps") {
    assertThat("horance",     equalTo("horance"))
    assertThat("horance liu", starts("horance"))
    assertThat("horance liu", ends("liu"))
    assertThat("horance liu", contains("horance"))
  }

  describe("StringOps: IgnoringCase") {
    assertThat("horance",     ignoring_case(equalTo)("HORANCE"))
    assertThat("horance liu", ignoring_case(starts)("HORANCE"))
    assertThat("horance liu", ignoring_case(ends)("LIU"))
    assertThat("horance liu", ignoring_case(contains)("HORANCE"))
  }

  describe("Comparations") {
    assertThat(2, lt(3))
    assertThat(2, gt(1))

    assertThat(2, gteq(1))
    assertThat(2, gteq(2))

    assertThat(2, lteq(3))
    assertThat(2, lteq(2))
    assertThat(2, equiv(2))
  }

  describe("BlankOrNil") {
    assertThat("  ", blank)
    assertThat(null, blank_or_nil)
  }

  describe("EmptyOrNil") {
    assertThat("", empty)
    assertThat(null, empty_or_nil)
  }
}

