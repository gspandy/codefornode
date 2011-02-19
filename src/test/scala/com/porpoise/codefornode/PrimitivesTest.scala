package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._
import java.util.Date

@RunWith(classOf[JUnitRunner])
class PrimitivesTest extends FunSuite {

  test("Primitives can match on string input") {
      "123" match { case INT(x) => assertEquals(123, x) }
      "12.23" match { case DEC(x) => assertEquals(BigDecimal("12.23"), x) }
      "2010/12/12" match { case DATE(x) => assertTrue(x.toString().contains("Sun Dec 12 00:00:00 GMT 2010")) }
      "" match { case STRING(x) => assertTrue(x.isEmpty()) }
  }
  test("The most appropriate primitive will be returned from a string") {
      assertEquals(INT, Primitive("123"))
      assertEquals(DEC, Primitive(".123"))
      assertEquals(DATE, Primitive("01.02.2011"))
      assertEquals(STRING, Primitive("123FourFiveSix"))
  }
  test("a integer can be promoted to a big decimal") {
  }
}