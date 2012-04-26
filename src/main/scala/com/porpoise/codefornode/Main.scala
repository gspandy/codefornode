package com.porpoise.codefornode

import java.io.File
import scala.xml.XML
import com.google.common.base.Charsets
import com.google.common.io.Files
import com.porpoise.codefornode.ui.CodeForNodePanel
import com.porpoise.codefornode.ui.TargetLanguage
import com.porpoise.codefornode.ui.TargetLanguage._
import com.porpoise.gen.beans.BeanGenerator
import com.porpoise.gen.beans.ScalaGenerator

object Main {

  def main(args: Array[String]) = {
    object Controller extends CodeForNodePanel.Controller {
      override def generate(xmlFile: File, destDir: File, pckg: String, lang: TargetLanguage) = {
        val inputXml = XML.loadFile(xmlFile)
        val root = CodeForNode(inputXml)
        val model = XmlToModel.typeToModel(pckg, root)
        Files.write(model.toXml(), new File(destDir, "model.xml"), Charsets.UTF_8)
        lang match {
          case Java => BeanGenerator.generateMain(model, destDir)
          case Scala => ScalaGenerator.generateMain(model, destDir)
        }

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
