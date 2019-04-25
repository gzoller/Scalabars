package co.blocke.scalabars

import co.blocke.scalajack._

case class Desc(heavy: String)
case class Data(
    name: String,
    msg:  String,
    A:    List[Desc]
)

object Runme extends App {

  val sj = ScalaJack(json4s.Json4sFlavor())

  //  val input = "Hello, {{name}} here msg wow."
  val input =
    """Hello, {{name}} here {{{msg}}} wow.
      |{{^bogus}}
      |I'm alive!
      |{{/bogus}}
      |{{#A}}
      |   some stuff {{heavy}} here
      |{{/A}}
    """.stripMargin

  val context = sj.render(Data("Greg", "we go", List(Desc("cool"), Desc("wicked")))).asInstanceOf[HB.Context]

  val p = HandlebarsParser()
  val output = p.compile(input).map(_.render(context)).mkString("")

  println(output)
  println("-------")
}
