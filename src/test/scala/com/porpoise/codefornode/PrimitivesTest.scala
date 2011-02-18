package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._
import java.util.Date

@RunWith(classOf[JUnitRunner])
class PrimitivesTest extends FunSuite {

  test("a big decimal can be inferred from a string") {
    
  }
  test("a date can be inferred from a string") {
    import DATE._
    val date : Date = DATE("2010-12-12").get
    println("DATE " * 80)
    println(date)
  }
  test("a integer can be promoted to a big decimal") {
  }
}