package com.porpoise.codefornode

import com.porpoise.gen.beans.model._
import scala.xml._
import Cardinality._
import com.porpoise.gen.common.Names

object XmlToModel {

  def apply(namespace: String, node: Node): BeanModel = typeToModel(namespace, CodeForNode(node))

  def typeToModel(namespace: String, t: XmlType): BeanModel = {

    def addDef(container: Namespace, newType: XmlType): BeanModel = {
      if (container.getDefinition(newType.name) == null) {
        if (!newType.isEmpty) {
          val newDef = container.addDefinition(newType.name)
          for (att <- newType.allAttributes) {
            val newField = newDef.addField(att.name)

            newField.setAnnotations("@XmlAttribute")
            att.attType match {
              case INT => newField.setType(Types.lng())
              case DEC => newField.setType(Types.decimal())
              case DATE => newField.setType(Types.date())
              case BOOL => newField.setType(Types.bool())
              case _ => newField.setType(Types.string())
            }
          }
          for (f <- newType.fields) {
            val newField = newDef.addField(f.name)

            newField.setAnnotations("@XmlElement")

            if (!f.fieldType.isEmpty) {
              newField.setType(f.fieldType.name)
            }
            if (f.cardinality == OneToMany) {
              newField.setCollectionType(CollectionType.LIST)
            }
          }
        }
      }

      newType.fields.foreach(f => addDef(container, f.fieldType))
      container.getModel()
    }

    addDef(new BeanModel().addNamespace(namespace), t)
  }

}