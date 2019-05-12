package co.blocke.scalabars
package builtins.misc

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.options.MutableDataSet

object MarkdownHelper {
  lazy val options = new MutableDataSet()

  // uncomment to set optional extensions
  //options.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create(), StrikethroughExtension.create()));

  // uncomment to convert soft-breaks to hard breaks
  //options.set(HtmlRenderer.SOFT_BREAK, "<br />\n");

  lazy val markdownParser = Parser.builder(options).build()
  lazy val htmlRenderer = HtmlRenderer.builder(options).build()
}

case class MarkdownHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    MarkdownHelper.htmlRenderer.render(MarkdownHelper.markdownParser.parse(options.fn()))
}
