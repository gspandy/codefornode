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
  lazy val cardString = if (cardinality == OneToOne) "1" else "*"
  override def toString = "%s:%s(%s)".format(name, fieldType.name, cardString)   
}

case class Type(name : String, simpleFields : Set[String] = Set.empty, fields : Seq[Field] = Nil) {
  def field(name : String) = fields.find(_.name == name).get
  lazy val types = fields.map(f => f.fieldType)
  lazy val isEmpty = (simpleFields - "text").isEmpty && fields.isEmpty 
  lazy val allSubtypes : Seq[Type] = types ++ fields.flatMap(f => f.fieldType.allSubtypes)
  lazy val uniqueSubtypes = allSubtypes.toSet
  lazy val allSubtypeNames = allSubtypes.map(_.name)
  lazy val complexFieldNames = fields.map(_.toString)
  override def toString = "%s [%s]".format(name, (simpleFields ++ complexFieldNames).mkString(",")) 
}

object Type {

  private def flattenFields(tn : String, allFields: Seq[Field]) : Seq[Field] = {
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
    val firstName = types.head.name
    assert(types.forall(t => t.name == firstName))
    val simples = types.flatMap(t => t.simpleFields)
    
    val fields = types.flatMap(t => flattenFields(t.name, t.fields))
    
    new Type(types.head.name, simples.toSet, fields) 
  }

   implicit def apply(xml : Node) : Type = normalize(newType(xml))

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

     /**
      * for all known types under the 'root' type, if any appear multiple times, then we have to merge their properties
      * together in order to get a complete picture
      */     
     def unifyTypes : Map[String, Type] = {
	     val typeByName = root.allSubtypes.groupBy(t => t.name)
	     var (singleTypes, multiTypes) = typeByName.partition{ case (name, types) => types.size == 1 }
	     
	     // now merge the duplicate elements (elements with the same names)
	     val flattenedMultiTypes = multiTypes.mapValues{ types => flattenTypes(types.toSeq) }
	     val flattenedSingleTypes = singleTypes.mapValues{ st =>flattenTypes( st ) }
	
	     // finally put the maps back together -- the original "singles" map and our newly flattened (merged) map
	     flattenedSingleTypes ++ flattenedMultiTypes
     }

     val typesByName = unifyTypes
     
     // update all fields with the unified types
     update(root) { oldType => typesByName(oldType.name) }
   }
      
   def newType(xml : Node) : Type = {
      val attFields = (attributes(xml).keySet + "text").toSet
      val children = xml.nonEmptyChildren.filter { 
        case e : Elem => true
        case other => false
      }
      
      children.foreach { case e : Elem => println("%s :: %s".format(name(xml), e.label ))}
      // create a new field for *every* child (we will have duplicates!)
      val allFields = children.map( child => new Field(name(child), newType(child)))
      
      new Type(name(xml), attFields, allFields)
   }

  def name(xml : NodeSeq) = xml match {
    case e : Elem => e.label
    case other => other.getClass.getSimpleName
  }

  def attributes(xml : Node) = xml.attributes.asAttrMap
}