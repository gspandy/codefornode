package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class CodeForNodeTest extends FunSuite {

  test("xml nodes at different levels will be merged and have the correct cardinality") {
    val xml = <root>
      <house>
        <property name="Pentagon" />
      </house>    
      <cars>
        <car>
          <property color="Red" />
          <property age="12" />
        </car>
        <car>
          <property doors="four" />
        </car>
      </cars>
      <a><b><c>
         <property alphabet="true" />
         <property yetAnother="property" />
      </c></b></a>
    </root>
    
    println("TEST " * 10)
    
    val root = Type(xml)
    
    println(root)
    println("TEST " * 10)
    
    val msg = "seven subtypes expected:" + root.uniqueSubtypes.mkString(",%n".format())
    assertEquals(msg, 7, root.uniqueSubtypes.size)
    val carField = root.field("cars").fieldType.field("car")
    assertEquals("The 'car' field in 'cars' should be one-to-many:" + carField, Cardinality.OneToMany, carField.cardinality)
  }
  
}