package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.junit.Assert
import org.scalatest.Spec
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._

@RunWith(classOf[JUnitRunner])
class XmlToModelTest extends Spec {
  describe("XmlToModelTest") {
    val ns = "some.namespace"

    it("should convert empty elements into an xml model") {
      // call the method under test
      val model = XmlToModel(ns, <foo title="some title"> <bar/></foo>)

      val namespace = model.getNamespace(ns)
      val foo = namespace.getDefinition("foo")
      assert("foo" === foo.getName)
      assert(2 === foo.getFieldCount)
      assert("String" === foo.getField("title").getType)
      assert("@XmlAttribute" === foo.getField("title").getAnnotations.head)

      val bar = foo.getField("bar")
      assert("String" === bar.getType, "simple element children should be considered string 'attributes' ")
    }
    it("should combine types for elements with the same names") {
      // call the method under test
      val topType = CodeForNode(<foo title="some title"> <bar/><nested><bar name="n" age="123"/></nested></foo>)
      println(topType)
      println(topType.field("bar").fieldType)
      val model = XmlToModel(ns, <foo title="some title"> <bar/><nested><bar name="n" age="123"/></nested></foo>)

      val foo = model.getNamespace(ns).getDefinition("foo")
      println(foo)
      val bar = foo.getField("bar")
      assert("%s.Bar".format(ns) === bar.getType, "the two 'bar' elements should have been merged to form a complex type")
    }
  }
}