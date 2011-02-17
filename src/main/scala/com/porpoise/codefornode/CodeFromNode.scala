package com.porpoise.codefornode

import scala.xml._

object Cardinality extends Enumeration {
  type Cardinality = Value
  val OneToOne, OneToMany = Value
} 

import Cardinality._

case class Field(name : String, fieldType : Type, cardinality : Cardinality = OneToOne, defaultValue: Option[String] = None)

case class Type(name : String, simpleFields : Set[String] = Set.empty, fields : Seq[Field] = Nil)

object Type {

   val xml = <books>
   <book name="Oliver Twist">
   <chapters>
   <chapter name="Introduction">
   <page title="page one">some content</page>
   <page id="an id">some more content</page>
   </chapter>
   </chapters>
   </book>
   </books>

  /**
   * A type may appear once as <book name='asdf' /> and elsewhere as <book id='123' />.
   * Here we merge the properties for all elements with similar names
   */
  def flattenTypes(types : Seq[Type]) : Type = {
    val first = types.head
    val simples = types.flatMap(t => t.simpleFields)
    val mergedType = new Type(first.name, simples.toSet) 
    mergedType
  }
  
   private def merge(fieldByName : Map[String, Seq[Field]], c : Cardinality) = {
      for ((name, fields) <- fieldByName ) yield {
         val merged = flattenTypes(fields.map(_.fieldType))
         new Field(name, merged, cardinality=c)
      }
   }
  
   implicit def apply(xml : Node) : Type = {
      val attFields = (attributes(xml).keySet + "text").toSet
      val children = xml.nonEmptyChildren.filter { 
        case e : Elem => true
        case other => false
      }
      // create a new field for *every* child (we will have duplicates!)
      val allFields = children.map( child => new Field(name(child), apply(child)))
      
      // convert all these children into a map of field names to fields  
      val fieldsByName = allFields.groupBy(f => f.name)
      val (singleFields, multiFields) = fieldsByName.partition{ case (name, fields) => fields.size == 1 }
      
      val flattenedSingleFields = merge(singleFields, OneToOne)
      val flattenedListFields = merge(multiFields, OneToMany)
      
      val fields = flattenedSingleFields ++ flattenedListFields  
      new Type(name(xml), attFields, fields.toSeq)
   }

  def name(xml : NodeSeq) = {
    xml match {
      case e : Elem => e.label
      case other => other.getClass.getSimpleName
    }
  }

  def attributes(xml : Node) = xml.attributes.asAttrMap

}