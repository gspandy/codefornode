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
          <speed>50</speed>
        </car>
      </cars>
      <a><b><c>
         <property alphabet="true" />
         <property yetAnother="property" />
      </c></b></a>
    </root>
    
    val root = Type(xml)
    
    val msg = "seven subtypes expected:" + root.uniqueSubtypes.mkString(",%n".format())
    assertEquals(msg, 8, root.uniqueSubtypes.size)
    val carField = root.field("cars").fieldType.field("car")
    assertEquals("The 'car' field in 'cars' should be one-to-many:" + carField, Cardinality.OneToMany, carField.cardinality)
  }
  
   test("multiple xml nodes can be parsed") {
  
    val xml = <root>
     <things name="more stuff, but only one thing this time">
       <item name="Gamma" />
     </things>    
     <things name="just some stuff">
       <item name="Alpha" />
       <item name="Beta" />
     </things>
    </root>
   
    val root = Type(xml)
   
    assertEquals("Root should have a 'things' field: " + root.fields, 1, root.fields.size)
    assertEquals("'things' should have a one-to-many cardinality", Cardinality.OneToMany, root.fields(0).cardinality)
   
    val thingsType = root.fields(0).fieldType
    assertEquals("'things' should have one complex field: " + thingsType.fields, 1, thingsType.fields.size)
    assertTrue("'things' should a 'name' attribute", thingsType.simpleFields.contains("name"))
   
    val itemType = thingsType.fields(0).fieldType
    assertEquals("the 'items' field should be one to many", Cardinality.OneToMany, thingsType.fields(0).cardinality)
    assertTrue("'items' should have no complex fields: " + itemType.fields, itemType.fields.isEmpty)
    assertTrue("'items' should have a 'name' attribute", itemType.simpleFields.contains("name"))
  }
}