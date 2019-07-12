package co.blocke.scalabars

import model._

object Runme extends App {

  val sb = Scalabars()

  val json = org.json4s.native.JsonMethods.parse("""
                                                   |{
                                                   |  "title": "My New Post",
                                                   |  "name": "Greg",
                                                   |  "thing": "name",
                                                   |  "age": 53,
                                                   |  "index": 15,
                                                   |  "ok": true,
                                                   |  "interests": [{
                                                   |    "item":"car",
                                                   |    "label":"Porsche 356"
                                                   |  },{
                                                   |    "item":"boat",
                                                   |    "label":"FPB 78"
                                                   |  }],
                                                   |  "strs": [
                                                   |    ["a","b","c"],
                                                   |    ["d","e"]
                                                   |  ],
                                                   |  "foo": ["Hello","World"],
                                                   |  "which": "myPartial",
                                                   |  "numbers":[5,6,7,8],
                                                   |  "numberSet":[[5,7],[8,9]],
                                                   |  "player":{
                                                   |    "name": "David",
                                                   |    "age": 12
                                                   |  },
                                                   |  "stuff":["a","b","c"]
                                                   |}""".stripMargin)

  val t =
    """{{# each strs as |ctx1 idx1 isFirst1 isLast1|}}
      |  outer: {{idx1}} first? {{isFirst1}}  last? {{isLast1}}
      |  {{# each . as |ct2 idx2 isFirst2 isLast2|}}
      |    inner: {{idx2}} first? {{isFirst2}}  last? {{isLast2}}
      |  {{/each}}
      |{{/each}}
    """.stripMargin

  println(sb.compile("Hey {{bogus}}!", Map("strict" -> true))(json))

  println("-----")

}

case class Scalar() extends Helper("a", "b", "c", "d") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val a = scalarArg("a")
    val b = scalarArg("b")
    val c = scalarArg("c")
    val d = scalarArg("d")
    List(a, b, c, d) == List(None, None, None, Some(53))
  }
}

case class Person(name: String, age: Int)
case class Desc(heavy: String)
case class Data(
    name:   String,
    msg:    String,
    aNum:   Int,
    isOK:   Boolean,
    small:  Long,
    A:      List[Desc],
    player: Person
)
case class Magic(name: String, stuff: Map[String, Int])

case class Stuff2(
    foo:   Map[String, String],
    bar:   Map[String, Int],
    thing: String
)
