package co.blocke.scalabars
package helpers.misc

import org.json4s._
import java.net.URL
import scala.util.{ Try, Success }
import model._

case class UrlHelper() extends Helper("url") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    Try(new URL(arg("url"))) match {
      case Success(url) =>
        val decomposed: Map[String, JValue] = Map(
          "protocol" -> nullSafe(() => url.getProtocol),
          "auth" -> nullSafe(() => url.getAuthority),
          "host" -> nullSafe(() => url.getHost),
          "port" -> JInt(url.getPort),
          "hostname" -> nullSafe(() => url.getHost),
          "hash" -> JInt(url.hashCode),
          "search" -> nullSafe(() => url.getQuery),
          "pathname" -> nullSafe(() => url.getPath),
          "path" -> nullSafe(() => url.getPath),
          "href" -> nullSafe(() => url.getRef)
        )
        val ctx = options.context
        ctx.value match {
          case j: JObject => options.fn(ctx.push(Merge.merge(j, new JObject(decomposed.toList)), "$urlLiberal"))
          case _          => throw new BarsException("UrlHelper must be used within an object context")
        }
      case _ => options.inverse()
    }

  private def nullSafe(fn: () => String): JValue =
    if (fn() == null)
      JNothing
    else
      JString(fn())
}
