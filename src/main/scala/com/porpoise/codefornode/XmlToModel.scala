package com.porpoise.codefornode

import com.porpoise.gen.beans.model._
import scala.xml._
import Cardinality._
import AnnotationType._

object XmlToModel {

  def xmlToModel(namespace : String, node : Node) = typeToModel(namespace, CodeForNode(node))

  def typeToModel(namespace : String, t : XmlType) = {
  
    def addDef(container : Namespace, newType : XmlType) : BeanModel = {
		if (container.getDefinition(newType.name) == null) {
	        if (!newType.isEmpty) {
			  val newDef = container.addDefinition(newType.name)
			  for (att <- newType.allAttributes) { 
			      val newField = newDef.addField(att.name)
			      val a = att.annotationType match {
			          case ATTRIBUTE => "@XmlAttribute"
			          case ELEMENT => "@XmlElement"
			      }
			      newField.setAnnotations(a)
			      
			  }
			  for (f <- newType.fields) {
			    val newField = newDef.addField(f.name)
			    
			    if (!f.fieldType.isEmpty) {
				    newField.setType(f.fieldType.name)
			    }
			    if (f.cardinality == OneToMany) {
			      newField.setCollectionType(CollectionType.LIST)
			    }
			  }
		    } else {
		      println("%s is empty... ignoring".format(newType.name))
		    }
        }
	    
	    newType.fields.foreach( f => addDef(container, f.fieldType) )
	    container.getModel()
    }
   
    addDef(new BeanModel().addNamespace(namespace), t)
  }

}