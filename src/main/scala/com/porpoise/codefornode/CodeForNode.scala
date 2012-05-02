package com.porpoise.codefornode
import scala.xml._

object Properties {
  val VersionMinor = 0
  val VersionMajor = 0
  val VersionBuild = 1

  val VersionString = "v%s.%s.%s".format(VersionMajor, VersionMinor, VersionBuild)
}

object Cardinality extends Enumeration {
  type Cardinality = Value
  val OneToOne, OneToMany = Value

  def mergeCardinality(a: Cardinality, b: Cardinality) = if (a == OneToMany || b == OneToMany) { OneToMany } else { OneToOne }
}
import Cardinality._

private[codefornode] case class Occurrence(min: Int, max: Int) {
  def this(instances: Int) = this(instances, instances)
  require(min <= max)
  require(min >= 0)

  lazy val cardinality: Cardinality = {
    if (max > 1)
      Cardinality.OneToMany
    else
      Cardinality.OneToOne
  }
  def required: Boolean = min > 0

  def merge(other: Occurrence): Occurrence = Occurrence(min.min(other.min), max.max(other.max))
}

case class XmlField(name: String,
  fieldType: XmlType,
  occurrences: Occurrence) {

  def cardinality = occurrences.cardinality
  def required = occurrences.required

  lazy val cardString = if (cardinality == OneToOne) "1" else "*"
  override def toString = "%s:%s(%s)".format(name, fieldType.name, cardString)

  def merge(other: XmlField) = {
    assert(name == other.name)
    assert(fieldType.name == other.fieldType.name)
    XmlField(name, fieldType, occurrences.merge(other.occurrences))
  }

}

case class XmlAttribute(name: String, attType: Primitive) {
  override def toString = "%s:%s".format(name, attType)
}
/** Within some XML, its xml elements are represented by XmlTypes */
trait XmlType {
  def name: String
  def attributes: Map[String, XmlAttribute] = Map.empty
  def fields: Seq[XmlField]

  lazy val allAttributes: Iterable[XmlAttribute] = attributes.values

  def field(name: String) = fieldOpt(name).getOrElse {
    val err = "field '%s' not found. Fields are: %s".format(name, fields.map(_.name).mkString("[", ",", "]"))
    throw new IllegalArgumentException(err)
  }
  def fieldByName = fields.groupBy(_.name).mapValues { v =>
    val Seq(only) = v.toSeq
    only
  }
  def fieldOpt(name: String) = fields.find(_.name == name)
  def types = fields.map(f => f.fieldType)
  def isEmpty = allAttributes.isEmpty && fields.isEmpty
  def allSubtypes: Seq[XmlType] = types ++ fields.flatMap(f => f.fieldType.allSubtypes)
  //lazy val uniqueSubtypes = allSubtypes.toSet
  lazy val allSubtypeNames = allSubtypes.map(_.name)
  lazy val complexFieldNames = fields.map(_.toString)

  override def toString = {
    "XmlType %s : %s%n\tFields:%n\t%s".format(name, attributes.values, fields.mkString(",%n\t".format()))
  }

  def merge(other: XmlType): XmlType
}

object Maps {
  // stolen from http://stackoverflow.com/questions/1262741/scala-how-to-merge-a-collection-of-maps
  def mergeMaps[K, V](allMaps: Map[K, V]*)(merge: (V, V) => V): Map[K, V] =
    (Map[K, V]() /: (for (map <- allMaps; entry <- map) yield entry)) { (mergedResult, entry) =>
      val key = entry._1
      mergedResult + (if (mergedResult.contains(key)) key -> merge(mergedResult(key), entry._2) else entry)
    }
}

/** Entry point for this utility. */
object CodeForNode {

  private class Type(
    override val name: String,
    override val attributes: Map[String, XmlAttribute],
    val fieldOccurrenceByName: Map[String, Occurrence],
    typeLookup: String => XmlType) extends XmlType {
    lazy val fieldsByName: Map[String, XmlField] = fieldOccurrenceByName map { case (name, value) => name -> XmlField(name, typeLookup(name), value) }
    lazy override val fields: Seq[XmlField] = fieldsByName.values.toSeq
    override def isEmpty = attributes.isEmpty && fieldsByName.isEmpty

    override def merge(other: XmlType) = {
      assert(other.name == name)

      val allAtts = attributes ++ other.attributes
      val mergedOccurrences = other match {
        case t: Type =>
          val mapOne = fieldOccurrenceByName
          val mapTwo = t.fieldOccurrenceByName
          (mapOne.keySet ++ mapTwo.keySet).map { fieldName =>
            val a = mapOne.getOrElse(fieldName, new Occurrence(0))
            val b = mapTwo.getOrElse(fieldName, new Occurrence(0))
            fieldName -> a.merge(b)
          }
      }

      val merged = new Type(name, allAtts, mergedOccurrences.toMap, typeLookup)
      merged
    }
  }

  def apply(xml: NodeSeq): XmlType = {
    val types = asTypes(xml)
    xml.head match { case e: Elem => types(e.label) }
  }

  /** convert the nodes to a one-to-may map of xml nodes by their element name */
  def nodesByName(nodes: NodeSeq): Map[String, List[Node]] = {
    var nodeByName: Map[String, List[Node]] = Map.empty
    def append(n: Elem) = nodeByName.updated(n.label, n :: nodeByName.getOrElse(n.label, Nil))
    nodes.foreach {
      case elem: Elem => nodeByName = Maps.mergeMaps(
        append(elem),
        nodesByName(elem.child)) { (nodesOne, nodesTwo) => nodesOne ::: nodesTwo }
      case _ => // ignore others
    }
    nodeByName
  }

  private[codefornode] def elemChildren(xml: Node) = xml.child.collect { case e: Elem => e }

  /** get the attributes as a map of attribute names to their primitive type */
  def attributes(xml: Node) = xml.attributes.asAttrMap.map { case (name, value) => name -> XmlAttribute(name, Primitive(value)) }

  /** try and convert all the given nodes to a primitive type*/
  def asPrimitiveOption(nodes: Seq[Node]): Option[Primitive] = {
    def primOpt(e: Node) = {
      if (elemChildren(e).isEmpty && e.attributes.isEmpty) {
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

  def asTypes(xml: NodeSeq): Map[String, XmlType] = {
    var typesByName: Map[String, XmlType] = Map.empty
    val typeLookup: String => XmlType = (key: String) => typesByName.apply(key)

    def newType(n: Node): XmlType = {
      val atts: Map[String, XmlAttribute] = attributes(n)
      val fieldOccurrenceByName: Map[String, Occurrence] = elemChildren(n) groupBy (_.label) mapValues (childNodes => new Occurrence(childNodes.size))
      new Type(n.label, atts, fieldOccurrenceByName, typeLookup = typeLookup)
    }

    def mergeXml(xml: Seq[Node]): XmlType = (newType(xml.head) /: xml.tail) { (xmlType, node) => xmlType.merge(newType(node)) }

    val nodesMap = nodesByName(xml)
    typesByName = nodesMap.mapValues(mergeXml _)
    typesByName
  }
}
