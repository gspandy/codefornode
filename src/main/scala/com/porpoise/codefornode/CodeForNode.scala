package com.porpoise.codefornode
import scala.xml._
import scala.collection._

object Cardinality extends Enumeration {
  type Cardinality = Value
  val OneToOne, OneToMany = Value
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
}
object XmlField {
    
    case class Field(override val name:String, override val fieldType:XmlType, override val cardinality : Cardinality) extends XmlField
    
    def apply(name : String, xmlType : XmlType, cardinality : Cardinality = OneToOne) = Field(name, xmlType, cardinality)
}

case class XmlAttribute(name : String, attType : Primitive, annotationType : AnnotationType = ATTRIBUTE)

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
  
  def merge(other : XmlType) = other
}

class Type(name : String = "", fieldNames : Map[String, Cardinality] = Map.empty, typeLookup : String => XmlType) extends XmlType {
  lazy override val fieldsByName : Map[String, XmlField] = fieldNames map { case (name, value) => name -> XmlField(name, typeLookup(name), value) }
  lazy override val fields : Seq[XmlField] = fieldsByName.values.toSeq
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
        nodesByName(elem.child)) { (nodesOne,nodesTwo) => 
           nodesOne ::: nodesTwo
        }
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
  def attributes(xml : Node) = xml.attributes.asAttrMap.mapValues(str => Primitive(str))
  
  
  def asTypes(xml : NodeSeq) : Map[String, XmlType] = {
    class Types {
      var typesByName : Map[String, XmlType] = Map.empty
      def addAll(types : Map[String, XmlType]) = typesByName = typesByName ++ types 
      def get(name : String) = typesByName(name)
    }
    val nodesMap = nodesByName(xml)
    
    val types = new Types()
    
    implicit def newType(n : Node) = new Type(name(n), typeLookup = types.get _)

    def mergeXml(xml : Seq[Node]) : XmlType = {
      val first : XmlType = xml.head
      (first /: xml) { (xmlType, node) => xmlType.merge(node) } 
    }

    types.addAll(nodesMap.mapValues(mergeXml _))
    types.typesByName
  }
}
