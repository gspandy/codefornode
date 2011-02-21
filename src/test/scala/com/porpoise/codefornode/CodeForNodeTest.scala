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

  test("an xml element can be converted to an XmlType") {
    val types = CodeForNode.asTypes(xml)
    
//    val names = types("root").allSubtypeNames
//    println(names)
    for (t <- types.values) {
        println("=" * 80)
        println(t)
        println("=" * 80)
    }
    
  }

  
  test("elemChildren returns a list of element xml child nodes") {
    val kids = CodeForNode.elemChildren((xml \\ "beta").toList.last)
    assertEquals(List("amount", "items", "items", "dave", "various", "booleanField"), kids.map(_.label).toList)
  }
  
  test("asPrimitiveOption can return a primitive for some xml") {
      val intXmlA = <integer>1</integer>
      val intXmlB = <i>16<subnode /></i>
      val intXmlC = <i inva="lid">16</i>
      
      val decXmlA = <dec>1.2</dec>
      val decXmlB = <d>1.6<subnode /></d>
      val decXmlC = <d ina="lid">1.6</d>
      
      val dateXmlA = <date>2010-10-10</date>
      val dateXmlB = <d>2010-10-10<subnode /></d>
      val dateXmlC = <d ina="lid">2010-10-10</d>
      
      val boolXmlA = <b>true</b>
      val boolXmlB = <b>false<subnode /></b>
      val boolXmlC = <b ina="lid">true</b>

      assertEquals(STRING, CodeForNode.asPrimitiveOption(List(intXmlA, decXmlA, dateXmlA,boolXmlA)).get)
      assertEquals(INT, CodeForNode.asPrimitiveOption(List(intXmlA, intXmlA)).get)
      assertEquals(DEC, CodeForNode.asPrimitiveOption(List(intXmlA, decXmlA)).get)
      assertEquals(BOOL, CodeForNode.asPrimitiveOption(List(boolXmlA, boolXmlA)).get)
            
      // assert any mixture of invalid elements result in a None
      assertFalse(CodeForNode.asPrimitiveOption(List(intXmlA, intXmlB)).isDefined)
      assertFalse(CodeForNode.asPrimitiveOption(List(intXmlA, intXmlC)).isDefined)
      assertFalse(CodeForNode.asPrimitiveOption(List(decXmlA, decXmlB)).isDefined)
      assertFalse(CodeForNode.asPrimitiveOption(List(decXmlA, decXmlC)).isDefined)
      assertFalse(CodeForNode.asPrimitiveOption(List(dateXmlA, dateXmlB)).isDefined)
      assertFalse(CodeForNode.asPrimitiveOption(List(dateXmlA, dateXmlC)).isDefined)
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