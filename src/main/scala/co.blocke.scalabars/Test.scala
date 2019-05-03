package co.blocke.scalabars

object Test extends App {

  /*
  val t =
    """
      |{{foo}}
      |{{foo bar}}
      |{{foo "this is a test"}}
      |{{foo "this is a test" blah}}
      |{{foo a = b}}
      |{{foo "this is a test" blah name=../person.name}}
      |{{foo "this is a test" blah name="Greg Zoller"}}
      |      |      """.stripMargin

   */
  val t = "Test {{{foo}}}"

  val sb = Scalabars().registerHelper("foo", """function(){ return Handlebars.escapeExpression("<b>hey</b>"); }""")
  println(sb.compile(t).render("nada"))

  /*
  val c = Data("Greg", "<p>Yay!</p>", List(Desc("cool"), Desc("wicked")), Person("Mike", 32))


  val u =
    """
      |Hello, {{foo name=A.[1].heavy}}!
    """.stripMargin

  val sb2 = ScalaBars(u).registerHelper("foo", """function() { return "FooBar "+this.name+"!"; }""")
  println(sb2.parsed)

  println(sb2.render(c))
   */
}
