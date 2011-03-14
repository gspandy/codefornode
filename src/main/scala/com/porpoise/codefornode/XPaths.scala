package com.porpoise.codefornode

import scala.xml._
import scala.collection._

object XPaths {

  def apply(xml : Elem) : List[String] = {

   def kids(elm : Elem) : List[Elem] = elm.child.toList.filter(e => e match {
     case e : Elem => true
     case other => false
   }) map {n => n.asInstanceOf[Elem]}

   def attributes(path : String, node : Elem) : List[String] = {
    val attributes = CodeForNode.attributes(node).keys
    attributes.map{ a=> "%s/@%s".format(path, a) }.toList
   }

   def occurrences(name : String, parent : Elem) = kids(parent) filter (e => e.label == name) size

   def xpaths(prefixPath : String, node : Elem) : List[String] = {
     val children = kids(node).map(e => e -> occurrences(e.label, node))

     val nodeAndXPath = children map { case (e, idx) => (e -> "%s/%s[%s]".format(prefixPath, e.label, idx)) }
     val current = attributes(prefixPath, node).toList
     val theRest = nodeAndXPath.flatMap { case (next, path) => xpaths(path, next) }
     if (theRest.isEmpty)
         return "%s/%s => %s".format(prefixPath, node.label, current.mkString(",")) :: current
     return current ++ theRest
   }
    val xpath = xml.label
    return xpaths(xpath, xml)
  }

}