package com.porpoise.codefornode

import scala.xml._

case class Field(name : String, fieldType : Type)

case class Type(name : String, fields : Seq[Field])

object CodeFromNode {

  def name(xml : NodeSeq) = {
    xml match {
      case e : Elem => e.label
      case other => other.getClass.getSimpleName
    }
  }

  def attributes(xml : Node) = xml.attributes.asAttrMap

  def asCode(xml : NodeSeq) = {

    val typeMap : Map[String,Type] = Map.empty

    

    typeMap
  }
}
