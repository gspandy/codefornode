package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class XPathsTest extends FunSuite {
    val xml = <root age="123" name="dave">
       <alpha />
       <alpha some="property">
         <beta occurrence="1" another="property">
            <amount>0.03</amount>
            <various>17</various>
            <booleanField>true</booleanField>
            <items name="here I appear once" />
         </beta>
         <beta second="occurrence">
            <amount>10</amount>
            <items name="here I appear a couple times" />
            <items name="see? I told you!" />
            <dave>
               <creationDate>2011-12-12</creationDate>
               <someNumber>4</someNumber>
            </dave>
            <various>here I'm some text</various>
            <various>another various element</various>
            <booleanField>false</booleanField>
            <dave>
               <someNumber>4</someNumber>
            </dave>
         </beta>
       </alpha>
    </root>


  test("xpaths can return a mapping of xpaths to their values") {
      val xpathsWithValues = XPaths.listXPathsWithValues(xml)
      println(xpathsWithValues.mkString("%n".format()))
      val byXPath = xpathsWithValues.toMap
      assertEquals(byXPath("root/alpha[2]/@some"), "property")
      assertEquals(byXPath("root/alpha[2]/beta[2]/booleanField[1]"), "false")
      assertEquals(byXPath("root/alpha[2]/beta[2]/items[2]/@name"), "see? I told you!")
  }
      
  test("xpaths returns a list of all xpaths and values") {
    val xpaths = XPaths(xml)

    val expected = """root/@age
                     |root/@name
                     |root/alpha[1]
                     |root/alpha[2]/@some
                     |root/alpha[2]/beta[1]/@occurrence
                     |root/alpha[2]/beta[1]/@another
                     |root/alpha[2]/beta[1]/amount[1]
                     |root/alpha[2]/beta[1]/items[1]
                     |root/alpha[2]/beta[1]/items[1]/@name
                     |root/alpha[2]/beta[1]/booleanField[1]
                     |root/alpha[2]/beta[1]/various[1]
                     |root/alpha[2]/beta[2]/@second
                     |root/alpha[2]/beta[2]/dave[1]/creationDate[1]
                     |root/alpha[2]/beta[2]/dave[1]/someNumber[1]
                     |root/alpha[2]/beta[2]/dave[2]/someNumber[1]
                     |root/alpha[2]/beta[2]/booleanField[1]
                     |root/alpha[2]/beta[2]/items[1]
                     |root/alpha[2]/beta[2]/items[1]/@name
                     |root/alpha[2]/beta[2]/items[2]
                     |root/alpha[2]/beta[2]/items[2]/@name
                     |root/alpha[2]/beta[2]/amount[1]
                     |root/alpha[2]/beta[2]/various[1]
                     |root/alpha[2]/beta[2]/various[2]""".stripMargin
     val xpathString = xpaths.mkString("%n".format())

     assertEquals(expected, xpathString)
  }

}