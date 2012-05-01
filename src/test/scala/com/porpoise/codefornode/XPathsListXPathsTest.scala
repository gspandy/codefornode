package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class XPathsListXPathTest extends FunSuite {

  def assertXPaths(actual: Set[String], expected: String*) = {
    val actualString = actual.mkString("[", ",", "]")
    val expectedString = expected.mkString("[", ",", "]")

    var diff = expected.toSet &~ actual
    assert(diff.isEmpty, "EXPECTED - ACTUAL: %s - %s = %s".format(expectedString, actualString, diff.mkString("[", ",", "]")))

    diff = actual &~ expected.toSet
    assert(diff.isEmpty, "ACTUAL - EXPECTED: %s - %s = %s".format(expectedString, actualString, diff.mkString("[", ",", "]")))

    assert(actual.size === expected.length, "%s != %s".format(expectedString, actualString))
  }

  test("XPaths.listXPaths will return the xpath for a root node") {
    assertXPaths(XPaths.listXPaths(<a></a>), "a")
  }
  test("XPaths.listXPaths will return the xpath for a root node with an attribute") {
    val actual = XPaths.listXPaths(<a foo="bar"></a>)
    assertXPaths(actual, "a", "a/@foo")
  }
  test("XPaths.listXPaths will return the xpath for a single nested node") {
    val actual = XPaths.listXPaths(<a><b/></a>)
    assertXPaths(actual, "a/b")
  }
  test("XPaths.listXPaths will return the xpath for a single nested node with an attribute") {
    val actual = XPaths.listXPaths(<a><b foo="bar"/></a>)
    assertXPaths(actual, "a/b", "a/b/@foo")
  }
  test("XPaths.listXPaths will return the xpath for a single nested node with multiple attributes") {
    val actual = XPaths.listXPaths(<a><b foo="bar" baz="chaz"/></a>)
    assertXPaths(actual, "a/b", "a/b/@foo", "a/b/@baz")
  }
  test("XPaths.listXPaths will return the xpath for two nested nodes") {
    val actual = XPaths.listXPaths(<a><b/><b/></a>)
    assertXPaths(actual, "a/b[1]", "a/b[2]")
  }
  test("XPaths.listXPaths will return the xpath for deeply nested nodes") {
    val actual = XPaths.listXPaths(
      <a>
        <z>zebra</z>
        <b foo="bar"><c>dee</c></b>
        <z>second z</z>
        <e>
          <f>
            <g h=""/>
          </f>
        </e>
        <z>
          <zz>zzz</zz>
        </z>
      </a>)

    //a/e/f,a,a/z[3],a/e

    assertXPaths(actual, "a/z[1]",
      "a/b/@foo",
      "a/b/c",
      "a/z[2]",
      "a/e/f/g",
      "a/e/f/g/@h",
      "a/z[3]/zz")
  }

}