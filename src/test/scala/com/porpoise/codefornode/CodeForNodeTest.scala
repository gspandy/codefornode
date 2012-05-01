package com.porpoise.codefornode

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.Assert
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec

@RunWith(classOf[JUnitRunner])
class CodeForNodeTest extends Spec {

  val xml = <root age="123" name="dave">
              <alpha some="property">
                <beta occurrence="1">
                  <amount>0.03</amount>
                  <various>17</various>
                  <booleanField>true</booleanField>
                  <items name="here I appear once"/>
                </beta>
                <beta second="occurrence">
                  <amount>10</amount>
                  <items name="here I appear a couple times"/>
                  <items name="see? I told you!"/>
                  <dave>
                    <creationDate>2011-12-12</creationDate>
                    <someNumber>4</someNumber>
                  </dave>
                  <various>here I'm some text</various>
                  <booleanField>false</booleanField>
                </beta>
              </alpha>
            </root>

  describe("CodeForNode.asTypes") {

    it("should convert string types to strings") {
      val foo = CodeForNode(<foo title="asdf"/>)
      assert("foo" === foo.name)
      val Seq(title) = foo.allAttributes.toSeq
      assert("title" === title.name)
      assert(STRING === title.attType)
    }
    it("should convert int types to ints") {
      val foo = CodeForNode(<foo int="123"/>)
      val Seq(integer) = foo.allAttributes.toSeq
      assert(INT === integer.attType)
    }
    it("should convert date types to dates") {
      val foo = CodeForNode(<foo date="30.12.2012"/>)
      val Seq(d8) = foo.allAttributes.toSeq
      assert(DATE === d8.attType)
    }
    def assertBar(bar: XmlType) {
      assert("bar" === bar.name)
      assert("name" === bar.attributes("name").name)
      assert(STRING === bar.attributes("name").attType)

      assert("age" === bar.attributes("age").name)
      assert(INT === bar.attributes("age").attType)

    }
    it("should combine type of the same name on the same level") {
      val types = CodeForNode.asTypes(<foo title="some title"> <bar/><bar name="n" age="123"/></foo>)
      val bar = types("bar")
      assertBar(bar)
    }
    it("should combine type of the same name on nested levels with different fields declared on each") {
      // elements of the same name on different levels should be considered the same 'type'
      val types = CodeForNode.asTypes(<foo title="some title"> <bar name="n"/><nested><bar age="123"/></nested></foo>)
      val bar = types("bar")
      assertBar(bar)
      assertBar(types("foo").field("bar").fieldType)
    }
    it("should combine type of the same name on nested levels") {
      // elements of the same name on different levels should be considered the same 'type'
      val types = CodeForNode.asTypes(<foo title="some title"> <bar/><nested><bar name="n" age="123"/></nested></foo>)
      val bar = types("bar")
      assertBar(bar)
      assertBar(types("foo").field("bar").fieldType)
    }
  }
  describe("CodeForNode.elemChildren") {
    it("should return a list of element xml child nodes") {
      val kids = CodeForNode.elemChildren((xml \\ "beta").toList.last)
      assertEquals(List("amount", "items", "items", "dave", "various", "booleanField"), kids.map(_.label).toList)
    }
  }

  describe("CodeForNode.asPrimitiveOption") {
    it("should return a primitive for some xml") {
      val intXmlA = <integer>1</integer>
      val intXmlB = <i>16<subnode/></i>
      val intXmlC = <i inva="lid">16</i>

      val decXmlA = <dec>1.2</dec>
      val decXmlB = <d>1.6<subnode/></d>
      val decXmlC = <d ina="lid">1.6</d>

      val dateXmlA = <date>2010-10-10</date>
      val dateXmlB = <d>2010-10-10<subnode/></d>
      val dateXmlC = <d ina="lid">2010-10-10</d>

      val boolXmlA = <b>true</b>
      val boolXmlB = <b>false<subnode/></b>
      val boolXmlC = <b ina="lid">true</b>

      assertEquals(STRING, CodeForNode.asPrimitiveOption(List(intXmlA, decXmlA, dateXmlA, boolXmlA)).get)
      assertEquals(INT, CodeForNode.asPrimitiveOption(List(intXmlA, <integer>2</integer>)).get)
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
  }

  describe("CodeForNode.nodesByName") {
    it("should return a map of all xml elements by their element name") {
      val nodesByName = CodeForNode.nodesByName(xml)
      val nodesNames = nodesByName.keySet

      val expected = List("root", "alpha", "beta", "dave", "amount", "creationDate", "someNumber", "items", "booleanField", "various")
      val diff = expected filterNot (nodesNames contains)
      assertEquals(expected.size, nodesNames.size)
      assertTrue("unexpected elements: " + diff, diff.isEmpty)
      assertEquals(1, nodesByName("root").size)
      assertEquals(2, nodesByName("beta").size)
      assertEquals(1, nodesByName("dave").size)
      assertEquals(1, nodesByName("alpha").size)
    }
  }

}