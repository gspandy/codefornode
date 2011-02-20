package com.porpoise.codefornode
import scala.xml._
import scala.collection._

object Cardinality extends Enumeration {
  type Cardinality = Value
  val OneToOne, OneToMany = Value
} 
import Cardinality._


trait XmlField {
  def name : String
  def fieldType : XmlType
  def cardinality : Cardinality = OneToOne
  
  lazy val cardString = if (cardinality == OneToOne) "1" else "*"
  override def toString = "%s:%s(%s)".format(name, fieldType.name, cardString)   
}

trait XmlType {
  def name : String
  def intAttributes : Set[String] = Set.empty
  def decimalAttributes : Set[String] = Set.empty
  def stringAttributes : Set[String] = Set.empty
  def dateAttributes : Set[String] = Set.empty
  def fields : Seq[XmlField]
  lazy val allAttributes = intAttributes ++ stringAttributes ++ dateAttributes
  
  def field(name : String) = fields.find(_.name == name).get
  lazy val types = fields.map(f => f.fieldType)
  lazy val isEmpty = allAttributes.isEmpty && fields.isEmpty 
  lazy val allSubtypes : Seq[XmlType] = types ++ fields.flatMap(f => f.fieldType.allSubtypes)
  lazy val uniqueSubtypes = allSubtypes.toSet
  lazy val allSubtypeNames = allSubtypes.map(_.name)
  lazy val complexFieldNames = fields.map(_.toString)
  
  def merge(other : XmlType) = other
}


case class Type(
  name : String = "",
  override val intAttributes : Set[String] = Set.empty,
  override val decimalAttributes : Set[String] = Set.empty,
  override val stringAttributes : Set[String] = Set.empty,
  override val dateAttributes : Set[String] = Set.empty,
  override val fields : Seq[XmlField] = Nil)
extends XmlType

object XmlType {

   implicit def apply(xml : Node) : XmlType = {
     new Type()
   }
   
   def asTypes(xml : NodeSeq) : Map[String, XmlType] = {
       val xmlByName = CodeForNode.nodesByName(xml)
       xmlByName.mapValues( CodeForNode.mergeXml(_) )
   }
}
import XmlType.apply

object Maps {
  // stolen from http://stackoverflow.com/questions/1262741/scala-how-to-merge-a-collection-of-maps
  def mergeMaps[K, V](allMaps: Map[K, V]*)(merge: (V, V) => V): Map[K, V] =
    (Map[K, V]() /: (for (map <- allMaps; entry <- map) yield entry)) { (mergedResult, entry) =>
    val key = entry._1
    mergedResult + (if (mergedResult.contains(key)) key -> merge(mergedResult(key), entry._2) else entry)
  }
}

class RichNode(xml : Elem) {

}
object RichNode {
  implicit def apply(xml : Elem) = new RichNode(xml)
}

/**
 *
 */
object CodeForNode {

  def apply(xml : NodeSeq) = {
    val types = asTypes(xml)
    xml.head match { case e : Elem => types(e.label) }
  }
  
  def name(xml : NodeSeq) = xml match {
    case e : Elem => e.label
    case other => other.getClass.getSimpleName
  }

  /** get the attributes as a map of attribute names to their primitive type */
  def attributes(xml : Node) = xml.attributes.asAttrMap.mapValues(str => Primitive(str))

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
  
  def mergeXml(xml : Seq[Node]) : XmlType = {
    val first : XmlType = new Type()
    (first /: xml) { (xmlType, node) => xmlType.merge(node) } 
  }
  
  def asTypes(xml : NodeSeq) : Map[String, XmlType] = {
    val nodesMap = nodesByName(xml)
    nodesMap.mapValues(mergeXml _)
  }
}
