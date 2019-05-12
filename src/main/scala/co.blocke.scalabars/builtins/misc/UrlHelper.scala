package co.blocke.scalabars
package builtins.misc

import org.json4s._
import java.net.URL
import scala.util.{ Try, Success }

case class UrlHelper() extends Helper(List("url")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    Try(new URL(resolve("url"))) match {
      case Success(url) =>
        val decomposed: Map[String, Context] = Map(
          "protocol" -> Context(JString(url.getProtocol)),
          "auth" -> Context(JString(url.getAuthority)),
          "host" -> Context(JString(url.getHost)),
          "port" -> Context(JInt(url.getPort)),
          "hostname" -> Context(JString(url.getHost)),
          "hash" -> Context(JInt(url.hashCode)),
          "search" -> Context(JString(url.getQuery)),
          "pathname" -> Context(JString(url.getPath)),
          "path" -> Context(JString(url.getPath)),
          "href" -> Context(JString(url.getRef))
        )
        val ctx = options.context.get
        options.fn(ctx.copy(extras = ctx.extras ++ decomposed))
      case _ => options.inverse()
    }
}
