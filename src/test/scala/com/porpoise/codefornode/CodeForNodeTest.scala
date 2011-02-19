package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class CodeForNodeTest extends FunSuite {
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
               <alpha hello="again" />
               <creationDate>2011-12-12</creationDate>
               <someNumber>4</someNumber>
            </dave>
            <various>here I'm some text</various>
            <booleanField>false</booleanField>
         </beta>
       </alpha>
    </root>


  test("asTypes can convert xml into XmlType representation") {
    val typesByName = CodeForNode.asTypes(xml)
  }

  test("an xml element can be converted to an XmlType") {
    val types = XmlType.asTypes(xml)("beta")
  }
  
  test("nodesByName returns a map of all xml elements by their element name") {

    val nodesByName = CodeForNode.nodesByName(xml)
    val nodesNames = nodesByName.keySet
    
    val expected = List("root", "alpha", "beta", "dave", "amount", "creationDate", "someNumber", "items", "booleanField", "various")
    val diff = expected filterNot (nodesNames contains)
    assertEquals(expected.size, nodesNames.size)    
    assertTrue("unexpected elements: " + diff, diff.isEmpty)    
    assertEquals(1, nodesByName("root").size)    
    assertEquals(2, nodesByName("beta").size)    
    assertEquals(1, nodesByName("dave").size)    
    assertEquals(2, nodesByName("alpha").size)
  }

}