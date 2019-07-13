package co.blocke

import scala.language.implicitConversions
import co.blocke.scalajack.ScalaJack
import co.blocke.scalajack.json4s.Json4sFlavor
import org.apache.commons.text.StringEscapeUtils

import scalabars.model._
import org.json4s._
import scala.reflect.runtime.universe.TypeTag

import collection.JavaConverters._
import org.graalvm.polyglot.proxy._
import org.graalvm.polyglot.Value

package object scalabars {

  type Path = List[String]

  implicit class OpsNum(val str: String) extends AnyVal {
    def isNumeric: Boolean = scala.util.Try(str.toDouble).isSuccess
  }

  lazy val sjJson = ScalaJack(Json4sFlavor())
  def toJson4s[T](t: T)(implicit tt: TypeTag[T]): JValue = sjJson.render(t)

  def prefixPath(pathPart: String, rest: String) = { if (pathPart == "/") "./" else pathPart } match {
    case pp if rest == "" => pp
    case pp if (pathPart.last == '/' || pathPart.last == '.') =>
      rest match {
        case _ if rest.startsWith("/") || rest.startsWith(".") => pp + rest.tail
        case _ => pp + rest
      }
    case pp => pp + "/" + rest
  }

  /**
   * Unwrap layers within EvaluationResults to base Scala values for comparison
   * @param er
   * @return
   */
  def normalizeResult(er: EvalResult[_]): Any = er match {
    case c: ContextEvalResult => c.value.value.values // ER->Context->JValue->Scala value
    case _                    => er.value
  }

  // <><><><><> Converters <><><><><>

  // 1) EvalResult --> String
  //--------------------------
  implicit def er2string(er: EvalResult[_])(implicit options: Options): String = er match {
    case c: ContextEvalResult => jv2string(c.value.value)
    case r: StringEvalResult if options.hash("noEscape").value == "false" => StringEscapeUtils.escapeHtml4(r.value)
    case _: NoEvalResult => ""
    case v => v.value.toString
  }

  // 2) JValue --> String
  //--------------------------
  implicit def jv2string(jv: JValue): String = jv match {
    case _: JObject => "[object Object]"
    case a: JArray  => a.arr.map(jv2string).mkString(",")
    case v: JValue  => v.values.toString
  }

  // 3) Scalars --> EvalResult (This allows helper returns in Scala to be casual.  They'll be converted to EvalResult.
  //--------------------------
  implicit def str2evalResult(s: String): EvalResult[_] = StringEvalResult(s)
  implicit def bool2evalResult(b: Boolean): EvalResult[_] = BooleanEvalResult(b)
  implicit def null2evalResult(n: Null): EvalResult[_] = NoEvalResult()
  implicit def long2evalResult(i: Int): EvalResult[_] = LongEvalResult(i)
  implicit def double2evalResult(d: Double): EvalResult[_] = DoubleEvalResult(d)
  implicit def list2evalResult(l: List[Any])(implicit partials: Map[String, Template]): EvalResult[_] =
    ContextEvalResult(Context.root(sjJson.render(l)).copy(partials = partials))
  implicit def map2evalResult(m: Map[String, Any])(implicit partials: Map[String, Template]): EvalResult[_] =
    ContextEvalResult(Context.root(sjJson.render(m)).copy(partials = partials))
  implicit def context2EvalResult(c: Context): EvalResult[_] = c.value match {
    case v: JBool   => BooleanEvalResult(v.values)
    case v: JInt    => LongEvalResult(v.values.toLong)
    case v: JLong   => LongEvalResult(v.values)
    case v: JDouble => DoubleEvalResult(v.values)
    case v: JString => StringEvalResult(v.values)
    case JNull      => NoEvalResult()
    case _          => ContextEvalResult(c)
  }

  // 4) EvalResult --> Context
  //--------------------------
  implicit def evalresult2context(er: EvalResult[_])(implicit base: Context): Context = er match {
    case s: StringEvalResult     => base.push(JString(s.value), "$erConversion")
    case s: SafeStringEvalResult => base.push(JString(s.value), "$erConversion")
    case c: ContextEvalResult    => c.value
    case b: BooleanEvalResult    => base.push(JBool(b.value), "$erConversion")
    case l: LongEvalResult       => base.push(JLong(l.value), "$erConversion")
    case d: DoubleEvalResult     => base.push(JDouble(d.value), "$erConversion")
    case NoEvalResult()          => Context.NotFound
  }

  // 5) EvalResult --> JValue
  //--------------------------
  implicit def evalresult2jvalue(er: EvalResult[_]): JValue = er match {
    case s: StringEvalResult     => JString(s.value)
    case s: SafeStringEvalResult => JString(s.value)
    case c: ContextEvalResult    => c.value.value
    case b: BooleanEvalResult    => JBool(b.value)
    case l: LongEvalResult       => JLong(l.value)
    case d: DoubleEvalResult     => JDouble(d.value)
    case NoEvalResult()          => JNothing
  }

  //--------<< Converters below this line are for JavaScript engine support >>-----------

  // 6) EvalResult --> Graal Value
  //------------------------------
  implicit def er2graalvalue(er: EvalResult[_])(implicit sb: Scalabars): Object = er match {
    case s: StringEvalResult     => sb.js.asValue(s.value)
    case s: SafeStringEvalResult => sb.js.asValue(s.value)
    case b: BooleanEvalResult    => sb.js.asValue(b.value)
    case l: LongEvalResult       => sb.js.asValue(l.value)
    case d: DoubleEvalResult     => sb.js.asValue(d.value)
    case NoEvalResult()          => null
    case c: ContextEvalResult    => jvalue2graalvalue(c.value.value)
  }

  // 7) JValue --> Graal Proxy
  //--------------------------
  implicit def jvalue2graalvalue(jv: JValue)(implicit sb: Scalabars): Object = {
    jv match {
      case ja: JArray =>
        val mapped = ja.arr.map(j => jvalue2graalvalue(j))
        ProxyArray.fromList(mapped.asJava)
      case jo: JObject =>
        val mapped = jo.obj.map {
          case (k, v) => k -> jvalue2graalvalue(v)
        }.toMap
        ProxyObject.fromMap(mapped.asJava)
      case js: JString =>
        sb.js.asValue(js.values)
      case jb: JBool =>
        sb.js.asValue(jb.values)
      case ji: JInt =>
        sb.js.asValue(ji.values.toInt)
      case jd: JDouble =>
        sb.js.asValue(jd.values)
      case t =>
        sb.js.asValue(jv.values)
    }
  }

  // 8) Graal Value --> EvalResult
  //------------------------------
  implicit def value2er(po: Value)(implicit context: Context): EvalResult[_] = po match {
    case v if v.isString                   => StringEvalResult(v.asString)
    case v if v.isBoolean                  => BooleanEvalResult(v.asBoolean)
    case v if v.isNull                     => NoEvalResult()
    case v if v.isNumber && v.fitsInInt    => LongEvalResult(v.asInt)
    case v if v.isNumber && v.fitsInLong   => LongEvalResult(v.asLong)
    case v if v.isNumber && v.fitsInDouble => DoubleEvalResult(v.asDouble)
    case v if v.hasArrayElements => // JValues --> Context --> (implicitly) ContextEvalResult
      context.push(JArray((0 until v.getArraySize().toInt).map(i => value2JValue(v.getArrayElement(i))).toList), "$js")
    case v if v.hasMembers =>
      context.push(JObject(v.getMemberKeys().asScala.toList.map(k => k -> value2JValue(v.getMember(k)))), "$js")
  }

  // 9) Graal Value --> JValue
  //--------------------------
  implicit def value2JValue(po: Value): JValue = po match {
    case v if v.isString                   => JString(v.asString)
    case v if v.isBoolean                  => JBool(v.asBoolean)
    case v if v.isNull                     => JNull
    case v if v.isNumber && v.fitsInInt    => JInt(v.asInt)
    case v if v.isNumber && v.fitsInLong   => JLong(v.asLong)
    case v if v.isNumber && v.fitsInDouble => JDouble(v.asDouble)
    case v if v.hasArrayElements           => JArray((0 until v.getArraySize.toInt).map(i => value2JValue(v.getArrayElement(i))).toList)
    case v if v.hasMembers =>
      JObject(v.getMemberKeys.asScala.toList.map { k =>
        k -> {
          v.getMember(k) match {
            case host if host.isHostObject =>
              host.asHostObject().asInstanceOf[AnyRef] match {
                case s: SafeStringEvalResult => JString(s.value)
                case _                       => throw new BarsException("Unknown object returned in data object from helper")
              }
            case other =>
              value2JValue(other)
          }
        }
      })
  }

  // 10) Map[String,EvalResult] --> Graal Value (ProxyObject)
  //---------------------------------------------------------
  implicit def maper2graalvalue(m: Map[String, EvalResult[_]])(implicit sb: Scalabars): ProxyObject =
    ProxyObject.fromMap(m.map { case (k, v) => k -> er2graalvalue(v) }.asJava)

  // 11) Graal Value (ProxyObject) --> Map[String,EvalResult]
  //---------------------------------------------------------
  implicit def graalvalue2maper(gv: Value)(implicit context: Context): Map[String, EvalResult[_]] = {
    if (!gv.hasMembers)
      throw new BarsException("Expected a data object")
    val keys = gv.getMemberKeys()
    if (!keys.contains("data"))
      throw new BarsException("'data' element missing from data object from Javascript")
    val jv: JValue = gv.getMember("data")
    jv match {
      case jo: JObject =>
        jo.obj.map {
          case (k, v) => k -> jv2er(v)
        }.toMap
      case x =>
        throw new BarsException("'data' element should be an object (JObject), not " + x)
    }
  }

  // 12) JValue --> EvalResult[_]
  //-----------------------------
  implicit def jv2er(jv: JValue)(implicit context: Context): EvalResult[_] = jv match {
    case v: JInt    => LongEvalResult(v.values.toInt)
    case v: JLong   => LongEvalResult(v.values)
    case v: JString => StringEvalResult(v.values)
    case v: JDouble => DoubleEvalResult(v.values)
    case JNull      => NoEvalResult()
    case v: JArray  => ContextEvalResult(context.push(v, "$js"))
    case v: JObject => ContextEvalResult(context.push(v, "$js"))
    case _          => NoEvalResult() // unsupported JValue type
  }
}
