package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s.JArray
import scala.util.{ Try, Success }

case class WithBeforeHelper() extends Helper("items", "loc") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    Try(scalarArg("loc").get.asInstanceOf[Long].toInt) match {
      case Success(loc) =>
        contextArg("items") match {
          case Some(c) => c.value match {
            case a: JArray =>
              a.arr.drop(loc).indices.map(i =>
                options.fn(lookup(c.path, s"[$i]"))
              ).mkString
            case _ => options.inverse()
          }
          case None => options.inverse()
        }
      case _ => options.inverse()
    }
}