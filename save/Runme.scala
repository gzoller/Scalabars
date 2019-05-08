package co.blocke.scalabars

case class Person(name: String, age: Int)
case class Desc(heavy: String)
case class Data(
    name:   String,
    msg:    String,
    A:      List[Desc],
    player: Person
)

object Runme extends App {

  //  val input = "Hello, {{name}} here msg wow."
  val input =
    """Hello, {{player.name}} here {{{msg}}} wow.
      |{{^bogus}}
      |{{!-- foo nothing --}}
      |I'm alive!
      |{{/bogus}}
      |{{#A}}
      |   some stuff {{heavy}} here
      |{{/A}}
    """.stripMargin

  /*
  val sb = ScalaBars(input) //.registerPartial("inclusion", "{{name}} was here!").registerHelper("foo", """function() { return "FooBar!"; }""")

  val c = Data("Greg", "we go", List(Desc("cool"), Desc("wicked")), Person("Mike", 32))
  println(sb.parsed)

   */

}
