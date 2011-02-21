package com.porpoise.codefornode

import com.porpoise.codefornode.ui._
import com.porpoise.gen.beans._
import scala.xml._
import scala.collection._
import java.io.File
import com.google.common.io.Files
import com.google.common.base.Charsets

object Main {

  def main(args : Array[String]) = {
    object Controller extends CodeForNodePanel.Controller {
      override def generate(xmlFile : File , destDir : File ,pckg : String ) = {
        val inputXml = XML.loadFile(xmlFile)
        val root = CodeForNode(inputXml)
        val model = XmlToModel.typeToModel(pckg, root)
        Files.write(model.toXml(), new File(destDir, "model.xml"), Charsets.UTF_8)
        BeanGenerator.generate(model, destDir)
        "success"
      }
    }
    CodeForNodePanel.showCodeForNode(Controller)
  }
}
