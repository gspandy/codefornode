package com.porpoise.xpaths

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.scalatest.Spec

@RunWith(classOf[JUnitRunner])
class XPathsDiffTest extends Spec {
  describe("XPaths.diff") {
    it("should return the xpath value differences between two nodes when only values differ") {
      //      val a = <a>valueA</a>
      //      val b = <b>valueB</b>
      //      val XPathDiff(onlyOnLeftDiff, valueDiff, onlyOnRightDiff) = XPaths.diff(a, b)
      //      assert(onlyOnLeftDiff.isEmpty)
      //      assert(onlyOnRightDiff.isEmpty)
      //      val (left, right) = valueDiff("b")
      //      assert("valueA" === left)
      //      assert("valueB" === left)
    }
    it("should return an empty difference between two nodes when no nodes or values") {
      //      val a = <a>same</a>
      //      val b = <b>same</b>
      //      val diff = XPaths.diff(a, b)
      //      assert(diff.isEmpty)
      //
      //      val XPathDiff(onlyOnLeftDiff, valueDiff, onlyOnRightDiff) = diff
      //      assert(onlyOnLeftDiff.isEmpty)
      //      assert(onlyOnRightDiff.isEmpty)
      //      assert(valueDiff.isEmpty)
    }
  }

}