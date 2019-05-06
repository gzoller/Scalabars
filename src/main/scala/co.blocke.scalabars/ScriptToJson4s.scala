package co.blocke.scalabars

import jdk.nashorn.api.scripting.JSObject
import org.json4s._

object ScriptToJson4s {

  def toJson4s(m: JSObject): JObject =
    JObject(m.keySet.toArray.map { k =>
      val key = k.toString
      (key -> elementToJson4s(m.getMember(key)))
    }.toList)

  private def arrayToJson4s(arr: JSObject): JArray =
    JArray(arr.values().toArray.toList.map(item => elementToJson4s(item)))

  private def elementToJson4s(element: Object) = element match {
    case null                         => JNull
    case s: String                    => JString(s)
    case i if i.isInstanceOf[Integer] => JInt(i.asInstanceOf[Int])
    case d if d.isInstanceOf[Double]  => JDouble(d.asInstanceOf[Double])
    case b if b.isInstanceOf[Boolean] => JBool(b.asInstanceOf[Boolean])
    case m2: JSObject if m2.isArray   => arrayToJson4s(m2)
    case m2: JSObject                 => toJson4s(m2)
    case _                            => JNothing
  }
}
