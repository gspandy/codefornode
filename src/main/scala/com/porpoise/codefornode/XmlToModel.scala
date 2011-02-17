package com.porpoise.codefornode

import com.porpoise.gen.beans.model._
import scala.xml._
import Cardinality._

object XmlToModel {

  def xmlToModel(namespace : String, node : Node) = typeToModel(namespace, Type(node))
  
  def typeToModel(namespace : String, t : Type) = {
  
    def addDef(container : Namespace, newType : Type) : BeanModel = {
		if (container.getDefinition(newType.name) == null) {
	        if (!newType.isEmpty) {
			  val newDef = container.addDefinition(newType.name)
			  for (att <- newType.simpleFields) { newDef.addField(att) }
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