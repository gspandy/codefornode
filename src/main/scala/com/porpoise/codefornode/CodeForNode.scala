package com.porpoise.codefornode
import scala.xml._
import scala.collection._

object Cardinality extends Enumeration {
  type Cardinality = Value
  val OneToOne, OneToMany = Value
  
  def mergeCardinality(a : Cardinality, b : Cardinality) = if (a == OneToMany || b == OneToMany) { OneToMany } else { OneToOne }
} 
import Cardinality._

object AnnotationType extends Enumeration {
  type AnnotationType = Value
  val ATTRIBUTE, ELEMENT = Value
} 
import AnnotationType._
import Cardinality._

trait XmlField {
  def name : String
  def fieldType : XmlType
  def cardinality : Cardinality = OneToOne
  
  lazy val cardString = if (cardinality == OneToOne) "1" else "*"
  override def toString = "%s:%s(%s)".format(name, fieldType.name, cardString)   
  
  def merge(other : XmlField) = {
    assert(name == other.name)
    assert(fieldType.name == other.fieldType.name)
    val c = mergeCardinality(cardinality, other.cardinality)
    XmlField(name, fieldType, c)
  }
}
object XmlField {
    case class Field(override val name:String, override val fieldType:XmlType, override val cardinality : Cardinality) extends XmlField
    def apply(name : String, xmlType : XmlType, cardinality : Cardinality = OneToOne) = Field(name, xmlType, cardinality)
}

case class XmlAttribute(name : String, attType : Primitive, annotationType : AnnotationType = ATTRIBUTE) {
  override def toString = "%s:%s[%s]".format(name, attType, annotationType)
}

trait XmlType {
  def name : String
  def attributes : Map[String, XmlAttribute] = Map.empty
  def fields : Seq[XmlField]
  lazy val allAttributes = attributes.values
  
  def field(name : String) = fields.find(_.name == name).get
  lazy val types = fields.map(f => f.fieldType)
  lazy val isEmpty = allAttributes.isEmpty && fields.isEmpty 
  lazy val allSubtypes : Seq[XmlType] = types ++ fields.flatMap(f => f.fieldType.allSubtypes)
  lazy val uniqueSubtypes = allSubtypes.toSet
  lazy val allSubtypeNames = allSubtypes.map(_.name)
  lazy val complexFieldNames = fields.map(_.toString)
  
  override def toString = {
    "XmlType %s : %s%n\t%s".format(name, attributes.values, fields.mkString(",%n\t".format()))
  }
  
  def merge(other : XmlType) : XmlType
  
  def merge2(other : XmlType) : XmlType = {
      assert(other.name == name)
      
      def flattenFields(fields : Seq[XmlField]) : XmlField = fields match {
        case head :: Nil => head
        case head :: tail => (head /: tail){ (merged, field) => merged.merge(field) }
      }
      
      val allAtts = attributes ++ other.attributes
      val allFields = fields ++ other.fields
      val fieldsByName = allFields.groupBy(_.name)
      val mergedFields = fieldsByName.map{ case (name, fields) => flattenFields(fields) }
      
      val merged = new XmlTypeImpl(name, allAtts, mergedFields.toSeq)
      merged
  }
}
private[codefornode] case class XmlTypeImpl(
    override val name : String,
    override val attributes : Map[String, XmlAttribute],
    override val fields : Seq[XmlField]
    ) extends XmlType {
    override def merge(other : XmlType) = merge2(other)
}

object Maps {
  // stolen from http://stackoverflow.com/questions/1262741/scala-how-to-merge-a-collection-of-maps
  def mergeMaps[K, V](allMaps: Map[K, V]*)(merge: (V, V) => V): Map[K, V] =
    (Map[K, V]() /: (for (map <- allMaps; entry <- map) yield entry)) { (mergedResult, entry) =>
    val key = entry._1
    mergedResult + (if (mergedResult.contains(key)) key -> merge(mergedResult(key), entry._2) else entry)
  }
}

/** */
object CodeForNode {

  class Type(
    override val name : String = "", 
    override val attributes : Map[String, XmlAttribute],
    val fieldNames : Map[String, Cardinality] = Map.empty, 
    typeLookup : String => XmlType) extends XmlType {
      lazy val fieldsByName : Map[String, XmlField] = fieldNames map { case (name, value) => name -> XmlField(name, typeLookup(name), value) }
      lazy override val fields : Seq[XmlField] = fieldsByName.values.toSeq

	  override def merge(other : XmlType) = {
	      assert(other.name == name)
	      
	      val allAtts = attributes ++ other.attributes
	      val allFieldNames = other match {
	         case t : Type => Maps.mergeMaps(fieldNames, t.fieldNames) { (first, second) =>
	           Cardinality.mergeCardinality(first, second)
	         
	         
/*
	         case other : XmlType => {
               val fieldsByName = allFields.groupBy(_.name)
               fieldsByName mapValues (fields
               
	         }
*/
	         
	         
	         } 
	      }
	      
	      val merged = new Type(name, allAtts, allFieldNames, typeLookup)
	      merged
	  }
  }

  def apply(xml : NodeSeq) = {
    val types = asTypes(xml)
    xml.head match { case e : Elem => types(e.label) }
  }

  /** convert the nodes to a one-to-may map of xml nodes by their element name */
  def nodesByName(nodes : NodeSeq) : Map[String, List[Node]] = {
    var nodeByName : Map[String, List[Node]] = Map.empty
    def append(n : Elem) = nodeByName.updated(n.label, n :: nodeByName.getOrElse(n.label, Nil))
    nodes.foreach { 
      case elem : Elem => nodeByName = Maps.mergeMaps( 
        append(elem), 
        nodesByName(elem.child)) { (nodesOne,nodesTwo) => nodesOne ::: nodesTwo }
      case node : Node => // ignore others
    }
    nodeByName
  }
  
  def partitionChildren(xml : Node) = xml.child.partition { 
    case e : Elem => true
    case other => false
  }
   
  def elemChildren(xml : Node) = partitionChildren(xml : Node)._1

  def name(xml : NodeSeq) = xml match {
    case e : Elem => e.label
    case other => other.getClass.getSimpleName
  }

  /** get the attributes as a map of attribute names to their primitive type */
  def attributes(xml : Node) = xml.attributes.asAttrMap.map{ case (name, value) => name -> XmlAttribute(name, Primitive(value)) }

  /** try and convert all the given nodes to a primitive type*/
  def asPrimitiveOption(nodes : Seq[Node]) : Option[Primitive] = {
      def primOpt(e : Node) = {
          if (elemChildren(e).size == 0 && e.attributes.isEmpty) {
              Some(Primitive(e.text))
          } else {
              None
          }
      }
      val first = primOpt(nodes.head)
      (first /: nodes.tail) { (option, elm) => 
        option match {
            case Some(p) => primOpt(elm).map(p.merge(_))
            case None => None
        }
      }
  }
  
  def asTypes(xml : NodeSeq) : Map[String, XmlType] = {
    var typesByName : Map[String, XmlType] = Map.empty

    val nodesMap = nodesByName(xml)
    
    def newType(n : Node) : XmlType = {
        var atts : Map[String, XmlAttribute] = attributes(n)
        var fieldNames : Map[String, Cardinality] = Map.empty
        val kidsByName = elemChildren(n) groupBy (_.label)        
        for ((name, childNodes) <- kidsByName) {
            val cardinality = if (childNodes.size == 1) { OneToOne } else { OneToMany }
            asPrimitiveOption(childNodes) match {
                case Some(primitiveType : Primitive) => atts = atts + (name -> XmlAttribute(name, primitiveType, ELEMENT))
                case None => fieldNames = fieldNames + (name -> cardinality)
            }
        }
        new Type(name(n), atts, fieldNames, typeLookup = typesByName.apply _)
    }

    def mergeXml(xml : Seq[Node]) : XmlType = (newType(xml.head) /: xml) { (xmlType, node) => xmlType.merge(newType(node)) } 

    // we have to do a first pass to ensure all the types are populated
    typesByName = typesByName ++ nodesMap.mapValues(nodes => newType(nodes.head))
    typesByName = typesByName ++ nodesMap.mapValues(mergeXml _)
    typesByName filterNot { case (k, v) => v.isEmpty }
  }
}
