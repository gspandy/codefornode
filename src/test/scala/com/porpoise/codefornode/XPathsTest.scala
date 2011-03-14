package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class XPathsTest extends FunSuite {
    val xml = <root age="123" name="dave">
       <alpha some="property">
         <beta occurrence="1">
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
            <booleanField>false</booleanField>
         </beta>
       </alpha>
    </root>

  test("xpaths returns a list of all xpaths and values") {
    val xpaths = XPaths(xml)

    val expected = """root/age
                     |root/name
                     |root/alpha[0]/some
                     |root/alpha[0]/beta[0]/occurrence
                     |root/alpha[0]/beta[0]/items[3]/name
                     |root/alpha[0]/beta[1]/@second
                     |root/alpha[0]/beta[1]/amount
                     |root/alpha[0]/beta[1]/items[1]/name
                     |root/alpha[0]/beta[1]/items[2]/name""".stripMargin
     val xpathString = xpaths.mkString("%n".format())

     println(xpathString)
     assertEquals(expected, xpathString)
  }
  
}