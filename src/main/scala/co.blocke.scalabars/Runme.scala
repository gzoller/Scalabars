package co.blocke.scalabars

import model._

object Runme extends App {

  val sb = Scalabars()

  val json = org.json4s.native.JsonMethods.parse(
    """
      |{
      |  "title": "My New Post",
      |  "name": "Greg",
      |  "age": 53,
      |  "ok": true,
      |  "interests": [{
      |    "item":"car",
      |    "label":"Porsche 356"
      |  },{
      |    "item":"boat",
      |    "label":"FPB 78"
      |  }],
      |  "foo": ["Hello","World"],
      |  "which": "myPartial",
      |  "numbers":[5,6,7,8],
      |  "numberSet":[[5,7],[8,9]],
      |  "player":{
      |    "name": "David",
      |    "age": 12
      |  },
      |  "stuff":["a","b","c"]
      |}
    """.stripMargin)

  /*
  val t =
    """{{#* inline "nombre"}}
      |A
      |  B -- {{name}}
      |C
      |{{/inline}}
      |My name is:
      |{{>nombre}}
      |  Say it loud!""".stripMargin

   */

  val t =
    """{{# name}}
      |What'cha {{this}} doin?
      |{{/name}}
      |Done""".stripMargin

  println(sb.compile(t)(json))

}

case class FooHelper() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    """This is <b>a</b> test
      |123
      |""".stripMargin
}

/*
Template:
     Text(Hello)
     Whitespace(
     )
     HelperTag name (PathHelper(name))
       args: List()
       contents:
         Text(What'cha doin?)
         Whitespace(
     )
     Whitespace(
     )
     Text(Done)
 */ 