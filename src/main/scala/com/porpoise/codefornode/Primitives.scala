package com.porpoise.codefornode
import java.util.Date

trait Primitive { self =>
  def merge(other : Primitive) : Primitive = {
    other match {
      case _ if (other.getClass.getSimpleName == self.getClass.getSimpleName) => other
      case _ => STRING
    } 
  }
}
object Primitive {
  implicit def apply(str : String) = {
    str match {
      case DATE(str) => DATE
      case DEC(str) => DEC
      case INT(str) => INT
      case STRING(str) => STRING
    }
  }
}
object INT extends Primitive {
  override def merge(other : Primitive) : Primitive = {
    other match {
      case DEC => DEC
      case _ => super.merge(other)
    } 
  }
  def unapply(str : String) = {
    try { 
      str.toInt
      Some(INT)
    } catch {
      case e => None
    }
  }
} 
object DEC extends Primitive {
  def unapply(str : String) = {
    try { 
      str.toDouble
      Some(DEC)
    } catch {
      case e => None
    }
  }
}
object STRING extends Primitive {
  def unapply(str : String) = Some(STRING)
}
object DATE extends Primitive {
  import java.text.{SimpleDateFormat => SDF}
  val formatters = List(new SDF("dd/MM/yyyy"),new SDF("yyyy/MM/dd"), new SDF("yyyy-MM-dd"),new SDF("dd-MM-yyyy")) 
  def parse(str : String) : Option[Date] = {
    for( f <- formatters) {
	    try { 
	      return Some(f.parse(str))
	    } catch {
	      case e => 
	    }
    }
    return None
  }
  implicit def apply(str : String) = parse(str)
  implicit def unapply(str : String) = {
    parse(str) match {
      case Some(_) => Some(DATE)
      case None => None
    }
  }
}