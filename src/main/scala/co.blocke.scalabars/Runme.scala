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
      |I'm alive!
      |{{> inclusion }}
      |{{/bogus}}
      |{{#A}}
      |   some stuff {{heavy}} here {{> (foo) }}
      |{{/A}}
    """.stripMargin

  val sb = ScalaBars(input).registerPartial("inclusion", "{{name}} was here!").registerHelper("foo", """function() { return "FooBar!"; }""")

  val c = Data("Greg", "we go", List(Desc("cool"), Desc("wicked")), Person("Mike", 32))
  val output = sb.render(c)

  println(output)
  println("-------")

  /*
  val player = "player/age"

  val playerC = context.find(player)
  println(playerC.value)
  val upC = playerC.find("../../msg")
  println(upC.value)

  println("---------")
  println(context.find("A.[1].heavy").value)
  */
}
