package com.porpoise.codefornode

import scala.xml._

object Cardinality extends Enumeration {
  type Cardinality = Value
  val OneToOne, OneToMany = Value
} 

import Cardinality._

case class Field(name : String, fieldType : Type, cardinality : Cardinality = OneToOne) {
  def attribute(index : Int) = fieldType.fields(index)
  def apply(index : Int) = fieldType.fields(index)
  def cardString = if (cardinality == OneToOne) "1" else "*"
  override def toString = "%s:%s(%s)".format(name, fieldType.name, cardString)   
}

case class Type(name : String, simpleFields : Set[String] = Set.empty, fields : Seq[Field] = Nil) {
  def types = fields.map(f => f.fieldType)
  def allSubtypes : Seq[Type] = types ++ fields.flatMap(f => f.fieldType.allSubtypes)
  def uniqueSubtypes = allSubtypes.toSet
  def allSubtypeNames = allSubtypes.map(_.name)
  def complexFieldNames = fields.map(_.toString)
  override def toString = "%s [%s]".format(name, (simpleFields ++ complexFieldNames).mkString(",")) 
}

object Type {

  private def flattenFields(allFields: Seq[Field]) : Seq[Field] = {
	  def merge(fieldByName : Map[String, Seq[Field]], c : Cardinality) = {
	      for ((name, fields) <- fieldByName ) yield {
	         val merged = flattenTypes(fields.map(_.fieldType))
	         new Field(name, merged, cardinality=c)
	      }
	  }
      // convert all these children into a map of field names to fields  
      val fieldsByName = allFields.groupBy(f => f.name)
      val (singleFields, multiFields) = fieldsByName.partition{ case (name, fields) => fields.size == 1 }
      
      val flattenedSingleFields = merge(singleFields, OneToOne)
      val flattenedListFields = merge(multiFields, OneToMany)
      
      val fields = flattenedSingleFields ++ flattenedListFields
      fields.toSeq  
  }

  /**
   * A type may appear once as <book name='asdf' /> and elsewhere as <book id='123' />.
   * Here we merge the properties for all elements with similar names
   */
  private def flattenTypes(types : Seq[Type]) : Type = {
    val simples = types.flatMap(t => t.simpleFields)
    val fields = flattenFields(types.flatMap(t => t.fields))
    new Type(types.head.name, simples.toSet, fields) 
  }

   implicit def apply(xml : Node) : Type = newType(xml)(apply _)

   /**
    * xml nodes may appear at various places/depths in the xml tree.
    * This function will find duplicate types and merge 'em, then reassign
    * the fields to point to the new merged types 
    */
   def normalize(root : Type) = {
     def update(root: Type)(f : Type => Type) : Type = {
       val newFields = root.fields.map{old =>
         val newType = update( f(old.fieldType) )(f)
         new Field(old.name, newType, old.cardinality)
       }
       new Type(root.name, root.simpleFields, newFields)
     }
     
     def unifyTypes : Map[String, Type] = {
		 // group all types by their names
	     val typeByName = root.allSubtypes.groupBy(t => t.name)
	     
	     // partition the list into singles (elements which have only appeared once) and 
	     // elements which have appeared multiple times 
	     var (singleFields, multiFields) = typeByName.partition{ case (name, types) => types.size == 1 }
	     
	     // now merge the duplicate elements (elements with the same names)
	     val flattenedTypes = multiFields.mapValues{ types => flattenTypes(types.toSeq) }
	
	     // finally put the maps back together -- the original "singles" map and our newly flattened (merged) map
	     val single : Map[String, Type] = singleFields.mapValues{ case head :: Nil => head }     
	     single ++ flattenedTypes
     }
     
     val allTypes = unifyTypes
     
     // update all fields with the unified types
     update(root){ oldType => allTypes(oldType.name) }
   }
      
   implicit def newType(xml : Node)( typeFactory : Node => Type) : Type = {

      val attFields = (attributes(xml).keySet + "text").toSet
      val children = xml.nonEmptyChildren.filter { 
        case e : Elem => true
        case other => false
      }
      // create a new field for *every* child (we will have duplicates!)
      val allFields = children.map( child => new Field(name(child), typeFactory(child)))

      new Type(name(xml), attFields, flattenFields(allFields))
   }

  def name(xml : NodeSeq) = xml match {
    case e : Elem => e.label
    case other => other.getClass.getSimpleName
  }

  def attributes(xml : Node) = xml.attributes.asAttrMap
}