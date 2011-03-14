package com.porpoise.codefornode

import scala.xml._
import scala.collection._

/**
 * Utility to list all XPaths for a given xml input
 */
object XPaths {

  def apply(xml : Elem) : List[String] = listXPaths(xml)
  
  /** create a list of all xpaths paired with their values */
  def listXPathsWithValues(xml : Elem) : List[(String, String)] = {
      import javax.xml.xpath._
      val xmlStr = xml.toString()
      val factory = XPathFactory.newInstance().newXPath()
      
      //TODO - very inefficient!
      def src = new org.xml.sax.InputSource(new java.io.StringReader(xmlStr)) 
      listXPaths(xml : Elem).map(xpath => xpath -> factory.compile(xpath).evaluate(src) )
  }
  /**
   * @return a list of xpath strings
   */
  def listXPaths(xml : Elem) : List[String] = {

   /** get the element children of a node */
   def kids(elm : Elem) : List[Elem] = elm.child.toList.filter(e => e match {
     case e : Elem => true
     case other => false
   }) map {n => n.asInstanceOf[Elem]}

   /** list the attribute names for a node with the given */
   def attributes(node : Elem) : List[String] = CodeForNode.attributes(node).keys.toList

   /** */
   def occurrences(name : String, parent : Elem) = kids(parent) filter (e => e.label == name) size

   def xpaths(prefixPath : String, node : Elem) : List[String] = {
     val childrenByName = kids(node).groupBy( _.label )

     val nodeAndXPath = for (list <- childrenByName.values; e <- list) yield { 
         val oneBasedIndex = 1 + list.indexOf(e)
         e -> "%s/%s[%s]".format(prefixPath, e.label, oneBasedIndex)
     }
     val current = attributes(node).map{ "%s/@%s".format(prefixPath, _) }.toList
     val theRest = nodeAndXPath.flatMap { case (next, path) => xpaths(path, next) }
     if (theRest.isEmpty)
         return prefixPath :: current
     return current ++ theRest
   }
   
   return xpaths(xml.label, xml)
  }

}