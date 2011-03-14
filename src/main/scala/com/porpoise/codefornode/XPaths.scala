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

   def xpaths(prefixPath : String, node : Elem) : List[String] = {
     val children = kids(node).zipWithIndex

println("%s kids: %n%s".format(node.label, children.map(_._1.label)))

     val nodeAndXPath = children map { case (e, idx) => (e -> "%s/%s[%s]".format(prefixPath, e.label, idx)) }
     val current = attributes(prefixPath, node).toList
     val theRest = nodeAndXPath.flatMap { case (next, path) => xpaths(path, next) }
     if (theRest.isEmpty)
         return "%s/%s".format(prefixPath, node.label) :: current
     return current ++ theRest
   }
    val xpath = xml.label
    return xpaths(xpath, xml)
  }

}