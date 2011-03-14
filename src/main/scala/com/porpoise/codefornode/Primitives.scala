package com.porpoise.codefornode
import java.util.Date

trait Primitive { self =>
  def merge(other : Primitive) : Primitive = {
    other match {
      case _ if (other.getClass.getSimpleName == self.getClass.getSimpleName) => other
      case _ => STRING
    } 
  }
  override def toString = getClass.getSimpleName.replace("$","")
}
object Primitive {
  implicit def apply(str : String) = {
    str match {
      case DATE(str) => DATE
      case INT(str) => INT
      case DEC(str) => DEC
      case BOOL(str) => BOOL
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
      Some(str.toInt)
    } catch {
      case e => None
    }
  }
}
object BOOL extends Primitive {
  def unapply(str : String) = {
    try { 
      Some(str.toBoolean)
    } catch {
      case e => None
    }
  }
} 
object DEC extends Primitive {
  def unapply(str : String) = {
    try { 
      Some(BigDecimal(str))
    } catch {
      case e => None
    }
  }
}
object STRING extends Primitive {
  def unapply(str : String) = Some(str)
}
object DATE extends Primitive {
  import java.text.{SimpleDateFormat => SDF}
  val formatters = List("yyyy.MM.dd", "dd.MM.yyyy", "dd/MM/yyyy","yyyy/MM/dd", "yyyy-MM-dd", "dd-MM-yyyy") map (new SDF(_)) 
  formatters.foreach(_.setLenient(false))
  def parse(str : String) : Option[Date] = {
    var option : Option[Date] = None
    for( f <- formatters; if (option == None) ) {
	    try { 
	      option = Some(f.parse(str))
	    } catch {
	      case e => 
	    }
    }
    return option
  }
  implicit def apply(str : String) = parse(str)
  implicit def unapply(str : String) = {
    parse(str) match {
      case Some(_) => Some(parse(str))
      case None => None
    }
  }
}