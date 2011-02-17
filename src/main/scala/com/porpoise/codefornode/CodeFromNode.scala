package com.porpoise.codefornode

import scala.xml._

case class Field(name : String, type : Type, defaultValue: Option[String] = None)

class Type(name : String, fields : Seq[Field] = Nil)

object Type {

   def apply(xml : NodeSeq) = {
     

   
   }

}


object CodeFromNode {

  def name(xml : NodeSeq) = {
    xml match {
      case e : Elem => e.label
      case other => other.getClass.getSimpleName
    }
  }

  def attributes(xml : Node) = xml.attributes.asAttrMap

  def asCode(xml : NodeSeq) : Map[String, Type] = {

    var typeMap : Map[String,Type] = Map.empty

    for (node <- xml;
         typeName = name(node);
         if (!typeName.contains("anon"))) {

       var t = typeMap.getOrElse(typeName, Type(typeName))
       var fields = attributes(node) map ( (k,v) => Field(k, Some(v)) )
       

       typeMap = typeMap + (typeName -> t)
       typeMap = typeMap ++ asCode(node)
    }

    typeMap
  }
}
