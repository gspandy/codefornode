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
import scala.xml.Node

object Main {

  def main(args: Array[String]) = {
    object Controller extends CodeForNodePanel.Controller {

      private def generateXml(inputXml: Node, destDir: File, pckg: String, lang: TargetLanguage): String = {
        try {
          val root = CodeForNode(inputXml)
          val model = XmlToModel.typeToModel(pckg, root)
          Files.write(model.toXml(), new File(destDir, "model.xml"), Charsets.UTF_8)
          lang match {
            case Java => BeanGenerator.generateMain(model, destDir)
            case Scala => ScalaGenerator.generateMain(model, destDir)
          }

          "Success: generated %s code under %s".format(lang, destDir.getPath)
        } catch {
          case e => "Error generating code: %s".format(e)
        }
      }

      override def generate(xml: String, destDir: File, pckg: String, lang: TargetLanguage): String = {
        try {
          val inputXml = XML.loadString(xml)
          generateXml(inputXml, destDir, pckg, lang)
        } catch {
          case e => "Invalid xml: %s".format(e.getMessage)
        }
      }
    }

    if (args.length == 1) {
      val xpaths = XPaths.apply(XML.loadFile(args(0)))
      xpaths.foreach(println)
    } else if (args.length > 1) {
      println("""Usage: Main [xml file]%n
                 |Either open the code-for-node UI (with no args) or, if given the location of an xml file,%n
                 |print its xpaths to standard out""".stripMargin.format())

    }

    if (args.isEmpty) {
      CodeForNodePanel.showCodeForNode(Controller, "CodeForNode", Properties.VersionString)
    }
  }
}
