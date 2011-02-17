package com.porpoise.codefornode

import scala.xml._

case class Field(name : String, fieldType : Type, defaultValue: Option[String] = None)

class Type(name : String, simpleFields : Seq[String] = Nil, fields : Seq[Field] = Nil)

object Type {

   def apply(xml : Node) : Type = {
      val attFields = attributes(xml).keySet.toSeq
      val fields = xml.nonEmptyChildren.map( child => new Field(name(child), apply(child))).toList
      new Type(name(xml), attFields, fields)
   }

  def name(xml : NodeSeq) = {
    xml match {
      case e : Elem => e.label
      case other => other.getClass.getSimpleName
    }
  }

  def attributes(xml : Node) = xml.attributes.asAttrMap

}