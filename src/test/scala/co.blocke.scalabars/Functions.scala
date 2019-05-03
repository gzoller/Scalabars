package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Functions() extends FunSpec with Matchers {

  val sb = Scalabars()
    .registerHelper("escape", """function(s) { return Handlebars.escapeExpression(s); }""")
    .registerHelper("safe", """function(s) { return Handlebars.SafeString(s); }""")

  describe("--------------------------\n:  Handlebars Functions  :\n--------------------------") {
    it("Handlebars.escapeExpression()") {
      sb.compile("""Hello, {{{escape "<b>ya</b>"}}}!""").render("") should equal("Hello, &lt;b&gt;ya&lt;/b&gt;!")
    }
    it("Handlebars.SafeString()") {
      sb.compile("""Hello, {{safe "<b>ya</b>"}}!""").render("") should equal("Hello, <b>ya</b>!")
    }
  }
}
