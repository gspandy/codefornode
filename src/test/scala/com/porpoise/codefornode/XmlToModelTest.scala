package com.porpoise.codefornode

import org.junit.runner.RunWith
import org.junit.Assert
import org.scalatest.Spec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class XmlToModelTest extends Spec {
  describe("XmlToModelTest") {
    it("should convert empty elemnts into an xml model") {
      val ns = "some.namespace"

      // call the method under test
      val model = XmlToModel(ns, <foo title="some title"> <bar/></foo>)

      val namespace = model.getNamespace(ns)
      val foo = namespace.getDefinition("foo")
      assert("foo" === foo.getName)
      assert(2 === foo.getFieldCount)
      assert("String" === foo.getField("title").getType)

      val bar = foo.getField("bar")
      assert("%s.Bar".format(ns) === bar.getType)
    }
  }
}