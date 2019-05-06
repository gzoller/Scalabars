package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Options() extends FunSpec with Matchers {

  val sb = Scalabars()
    .registerHelper("escape", """function(s) { return s; }""")
    .registerHelper("escape2", """function() { return this.bogus; }""")

  describe("------------------------\n:  Handlebars Options  :\n------------------------") {
    it("noEscape") {
      sb.compile("""Hello, {{escape "<b>ya</b>"}}!""", Map("noEscape" -> true)).render("") should equal("Hello, <b>ya</b>!")
    }
    it("strict") {
      the[Exception] thrownBy sb.compile("""Hello, {{xage}}!""", Map("strict" -> true)).render(Person("Greg", 53)) should have message "strict failed (missing field xage)"
      sb.compile("""Hello, {{xage}}!""", Map("strict" -> false)).render(Person("Greg", 53)) should equal("Hello, !")
      sb.compile("""Hello, {{escape xage}}!""", Map("strict" -> false)).render(Person("Greg", 53)) should equal("Hello, !")
    }
  }
}