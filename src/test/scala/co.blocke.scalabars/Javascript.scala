package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Javascript() extends FunSpec with Matchers {

  val sb = Scalabars()
  val c = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List(Desc("cool"), Desc("wicked")), Person("Mike", 32))

  describe("------------------------\n:  Javascript Helpers  :\n------------------------") {
    it("Simple no-arg, non-block script (escaped)") {
      val sb2 = sb.registerHelper("ping", """function(){ return "<p>pong</p>"; }""")
      sb2.compile("""Game: {{ping}} over""").render("") should be("Game: &lt;p&gt;pong&lt;/p&gt; over")
    }
    it("Simple no-arg, non-block script (un-escaped)") {
      val sb2 = sb.registerHelper("ping", """function(){ return "<p>pong</p>"; }""")
      sb2.compile("""Game: {{{ping}}} over""").render("") should be("Game: <p>pong</p> over")
    }
    it("Simple no-arg, non-block script (un-escaped with SafeString)") {
      val sb2 = sb.registerHelper("ping", """function(){ return Handlebars.SafeString("<p>pong</p>"); }""")
      sb2.compile("""Game: {{ping}} over""").render("") should be("Game: <p>pong</p> over")
    }
    it("Simple no-arg, non-block script (escaped with escapeExpression)") {
      val sb2 = sb.registerHelper("ping", """function(){ return Handlebars.escapeExpression("<p>pong</p>"); }""")
      sb2.compile("""Game: {{{ping}}} over""").render("") should be("Game: &lt;p&gt;pong&lt;/p&gt; over")
    }
    it("Helper that relies on 'this' context") {
      val sb2 = sb.registerHelper("ping", """function(){ return this.name; }""")
      sb2.compile("""Game: {{ping}} over""").render(c) should be("Game: Greg over")
    }
    it("Helper that relies on 'this' context (more complex path)") {
      val sb2 = sb.registerHelper("ping", """function(){ return this.player.age; }""")
      sb2.compile("""Game: {{ping}} over""").render(c) should be("Game: 32 over")
    }
    it("Helper that relies on 'this' context (more complex path) - 2 ") {
      val sb2 = sb.registerHelper("ping", """function(){ return this.A[1].heavy; }""")
      sb2.compile("""Game: {{ping}} over""").render(c) should be("Game: wicked over")
    }
    it("Helper that passes a parameter (simple literal)") {
      val sb2 = sb.registerHelper("ping", """function(a){ return a; }""")
      sb2.compile("""Game: {{ping "foo"}} over""").render(c) should be("Game: foo over")
    }
    it("Helper that passes a parameter (simple literal) - 2") {
      val sb2 = sb.registerHelper("ping", """function(a){ return a; }""")
      sb2.compile("""Game: {{ping false}} over""").render(c) should be("Game: false over")
    }
    it("Helper that passes a parameter (simple literal) - 3") {
      val sb2 = sb.registerHelper("ping", """function(a){ return a; }""")
      sb2.compile("""Game: {{ping 12.34}} over""").render(c) should be("Game: 12.34 over")
    }
    it("Helper that passes a parameter (path-simple)") {
      val sb2 = sb.registerHelper("ping", """function(a){ return a; }""")
      sb2.compile("""Game: {{ping player.name}} over""").render(c) should be("Game: Mike over")
    }
    it("Helper that passes a parameter (path-complex)") {
      val sb2 = sb.registerHelper("ping", """function(a){ return a; }""")
      sb2.compile("""Game: {{ping A.[0].heavy}} over""").render(c) should be("Game: cool over")
    }
    it("Helper that passes a parameter (path-complex w/deref in helper)") {
      val sb2 = sb.registerHelper("ping", """function(a){ return a.heavy; }""")
      sb2.compile("""Game: {{ping A.[0]}} over""").render(c) should be("Game: cool over")
    }
    it("Helper using Options object (passed in) -- hash/assignment variable") {
      val sb2 = sb.registerHelper("ping", """function(options){ return options.hash('label'); }""")
      sb2.compile("""Game: {{ping label="almost"}} over""").render(c) should be("Game: almost over")
    }
    it("Helper using Options object with parameter -- 'each' example") {
      val sb2 = sb.registerHelper("jsEach", """
                                  |function(items, options) {
                                  |  var ret = "";
                                  |
                                  |  for(var i=0, j=items.length; i<j; i++) {
                                  |    ret = ret + options.fn(items[i]);
                                  |  }
                                  |
                                  |  return ret;
                                  |}""".stripMargin)

      sb2.compile("""Hello, {{#jsEach A}}Is this {{heavy}}?{{/jsEach}}!""").render(c) should be("Hello, Is this cool?Is this wicked?!")
    }
  }
}