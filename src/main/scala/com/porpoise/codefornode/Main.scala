package com.porpoise.codefornode

import com.porpoise.codefornode.ui._
import com.porpoise.gen.beans._
import scala.xml._
import scala.collection._
import java.io.File
import com.google.common.io.Files
import com.google.common.base.Charsets

object Main {

  def main(args: Array[String]) = {
    object Controller extends CodeForNodePanel.Controller {
      override def generate(xmlFile: File, destDir: File, pckg: String) = {
        val inputXml = XML.loadFile(xmlFile)
        val root = CodeForNode(inputXml)
        val model = XmlToModel.typeToModel(pckg, root)
        Files.write(model.toXml(), new File(destDir, "model.xml"), Charsets.UTF_8)
        BeanGenerator.generateMain(model, destDir)
        "success"
      }
    }

    if (args.length == 1)
      XPaths.apply(XML.loadFile(args(0)))
    else if (args.length > 1)
      println("Usage: Main [xml file]%n\tEither open the code-for-node UI (with no args) or, if given the location of an xml file,%n\t print its xpaths to standard out".format())

    if (args.length == 0)
      CodeForNodePanel.showCodeForNode(Controller)
  }
}
