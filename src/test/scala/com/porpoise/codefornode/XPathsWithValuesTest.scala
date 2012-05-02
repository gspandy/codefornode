package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class XPathsWithValuesTest extends FunSuite {

  def assertXPathsAndValues(pathsAndValues: Map[String, String], expected: (String, String)*) = {
    val SEP = "%n".format()
    assert(pathsAndValues.size === expected.length, pathsAndValues.mkString(SEP, SEP, SEP) + " did not match " + expected.mkString(SEP, SEP, SEP))
    for ((k, v) <- expected) {
      val actual = pathsAndValues(k)
      assert(v === actual)
    }
  }

  test("XPaths.xpathsWithValues will return a mapping of xpaths to values for a single empty node") {
    val pathsAndValues = XPaths.xPathsWithValues(<a></a>)
    assertXPathsAndValues(pathsAndValues, "a" -> "")
  }

  test("XPaths.xpathsWithValues will return a mapping of xpaths to values for a single node with a value") {
    val pathsAndValues = XPaths.xPathsWithValues(<a>some value</a>)
    assertXPathsAndValues(pathsAndValues, "a" -> "some value")
  }

  test("XPaths.xpathsWithValues will return a mapping of xpaths to values for a single attribute") {
    val pathsAndValues = XPaths.xPathsWithValues(<a foo="bar"></a>)
    assertXPathsAndValues(pathsAndValues, "a" -> "", "a/@foo" -> "bar")
  }

  test("XPaths.xpathsWithValues will return a mapping of xpaths to values for a nested node") {
    val pathsAndValues = XPaths.xPathsWithValues(<a><nested>bar</nested></a>)
    assertXPathsAndValues(pathsAndValues, "a/nested" -> "bar")
  }

  test("XPaths.xpathsWithValues will return a mapping of xpaths to values for a nested node with an attribute") {
    val pathsAndValues = XPaths.xPathsWithValues(<a><nested att="ribute">bar</nested></a>)
    assertXPathsAndValues(pathsAndValues, "a/nested" -> "bar", "a/nested/@att" -> "ribute")
  }

  test("XPaths.xpathsWithValues will return a mapping of xpaths to values for a mixed node") {

    //[a/nested/@att -> ribute
    //a/nested -> bar
    //a -> mixedbar]
    val xml = <a>mixed<nested att="ribute">bar</nested></a>
    val pathsAndValues = XPaths.xPathsWithValues(xml)
    val dave = XPaths.xpathValues(xml)(Set("a/@text"))
    println(dave.mkString("[", "%n".format(), "]"))
    //    println(pathsAndValues.mkString("[", "%n".format(), "]"))
    //    assertXPathsAndValues(pathsAndValues, "a" -> "mixed", "a/nested" -> "bar", "a/nested/@att" -> "ribute")
  }

}