package com.porpoise.codefornode

import scala.xml._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

case class XPathDiff(
  onlyOnLeft: Map[String, String],
  differentValues: Map[String, (String, String)],
  onlyOnRight: Map[String, String]) {
  lazy val isEmpty = onlyOnLeft.isEmpty && onlyOnRight.isEmpty && differentValues.isEmpty
}

/**
 * Utility to list all XPaths for a given xml input
 */
object XPaths {

  def apply(xml: Elem): Set[String] = listXPaths(xml)

  /** create a list of all xpaths paired with their values */
  def xPathsWithValues(xml: Elem): Map[String, String] = {
    val xpaths = listXPaths(xml)
    xpathValues(xml)(xpaths)
  }

  /**
   *
   * @return the values for the given xpaths
   */
  def xpathValues(xml: Elem)(xpaths: Set[String]) = {
    import javax.xml.xpath._

    val factory = XPathFactory.newInstance().newXPath()

    //TODO - timings/memory profiling as this may be quite inefficient
    def src = new org.xml.sax.InputSource(new java.io.StringReader(xml.toString()))
    xpaths.map(xpath => xpath -> factory.compile(xpath).evaluate(src)).toMap
  }

  /**
   * diff two elements
   */
  def diff(lhs: Elem, rhs: Elem): XPathDiff = {
    val leftPathsAndValues = xPathsWithValues(lhs)
    val leftKeys = leftPathsAndValues.keySet

    val rightPathsAndValues = xPathsWithValues(rhs)
    val rightKeys = rightPathsAndValues.keySet

    val onlyOnLeft = {
      val keysOnlyInLeft = leftKeys &~ rightKeys
      leftPathsAndValues filterKeys (keysOnlyInLeft contains _)
    }

    val onlyOnRight = {
      val keysOnlyInRight = rightKeys &~ leftKeys
      rightPathsAndValues filterKeys (keysOnlyInRight contains _)
    }

    val diffPairs: Iterable[(String, (String, String))] = for {
      key <- leftKeys & rightKeys
      leftValue <- leftPathsAndValues.get(key)
      rightValue <- rightPathsAndValues.get(key)
      if leftValue != rightValue
    } yield {
      key -> (leftValue, rightValue)
    }

    new XPathDiff(onlyOnLeft, diffPairs.toMap, onlyOnRight)
  }
  /**
   * @return a list of xpath strings
   */
  def listXPaths(xml: Elem): Set[String] = {

    /** get the element children of a node */
    def kids(elm: Elem): Seq[Elem] = elm.child.toSeq.collect { case e: Elem => e }

    /** list the attribute names for a node with the given */
    def attributes(node: Elem): Set[String] = {

      /** get the attributes as a map of attribute names to their primitive type */
      def attributesForNode(xml: Node) = xml.attributes.asAttrMap.map { case (name, value) => name -> XmlAttribute(name, Primitive(value)) }

      attributesForNode(node).keySet
    }

    // keep track of the child x of N for each child
    // for example, given the xml:
    // <foo>
    //   <a />
    //   <bee />
    //   <a />
    //   <bee />
    // </foo>
    //
    // there are two 'a' children with a 'bee' element in between, leading to the 'a' xpaths of:
    // foo/a[1]
    // foo/bee[1]
    // foo/a[2]
    // foo/bee[2]
    def xpathsRecursive(prefixPath: String, xmlNode: Elem): Set[String] = {
      type NodeXpathAndCount = (Elem, String, Int)
      case class Result(
        occurrenceCounterByNodeName: Map[String, Int] = Map.empty,
        childNodesAndXPath: List[NodeXpathAndCount] = Nil) {

        def update(child: Elem): Result = {
          val nodeName = child.label
          val newCount = occurrenceCounterByNodeName.getOrElse(nodeName, 0) + 1
          val newMap = occurrenceCounterByNodeName.updated(nodeName, newCount)
          val xpath = "%s/%s[%s]".format(prefixPath, nodeName, newCount)
          Result(newMap, (child, xpath, newCount) :: childNodesAndXPath)
        }

        /**
         * Filter and transform any children which only have one occurrence
         */
        lazy val nodesAndXPaths: Iterable[(Elem, String)] = {
          val nodeNamesWithOnlyOneOccurrence: Set[String] = occurrenceCounterByNodeName.filter { case (_, v) => v == 1 }.keySet
          childNodesAndXPath map {
            case (node, xpath, 1) if nodeNamesWithOnlyOneOccurrence contains (node.label) => node -> "%s/%s".format(prefixPath, node.label)
            case (node, xpath, _) => node -> xpath
          }
        }
      }

      val nodeAndXpaths: Iterable[(Elem, String)] = (Result() /: kids(xmlNode)) { case (workingResult, next) => workingResult.update(next) }.nodesAndXPaths

      val attributeXPaths = attributes(xmlNode).map { "%s/@%s".format(prefixPath, _) }
      val theRest = nodeAndXpaths.flatMap { case (node, path) => xpathsRecursive(path, node) }

      // is there a text node - or are we on a leaf node? If so, include this node in our result
      val includePrefix: Boolean = {
        val found: Option[Node] = xmlNode.child.find {
          case t: scala.xml.Text => !t.text.trim.isEmpty()
          case _ => false
        }
        theRest.isEmpty || found.isDefined
      }

      // the xpaths are the attribute xpaths + this node (if we need to include it)
      val xpaths: Set[String] = if (includePrefix) {
        attributeXPaths + prefixPath
      } else {
        attributeXPaths
      }
      attributeXPaths ++ theRest ++ xpaths
    }

    return xpathsRecursive(xml.label, xml)
  }

}