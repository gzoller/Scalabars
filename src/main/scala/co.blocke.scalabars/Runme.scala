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
                                                   |}""".stripMargin)

  val js2 = org.json4s.native.JsonMethods.parse("""{
                                                  |  "stuff": [
                                                  |    [
                                                  |      {
                                                  |      "name": "Greg",
                                                  |      "age": 53
                                                  |      },
                                                  |      {
                                                  |      "name": "Lili",
                                                  |      "age": 44
                                                  |      }
                                                  |    ],
                                                  |    [
                                                  |      {
                                                  |      "name": "Mike",
                                                  |      "age": 34
                                                  |      },
                                                  |      {
                                                  |      "name": "Sally",
                                                  |      "age": 33
                                                  |      }
                                                  |    ]
                                                  |  ]
                                                  |}""".stripMargin)

  val c2 = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List.empty[Desc], Person("Mike", 32))
  println(sb.compile("""Hello, {{#withLast A}}thing {{this.heavy}}{{/withLast}} End!""")(c2))

  println("-----")

}

case class FooHelper() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    """This is <b>a</b> test
      |123
      |""".stripMargin
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

/*

  val t =
    """Testing...{{#name}}
      |  {{#each ../interests}}
      |    {{#*inline "myPartial"}}
      |      Bar!
      |      Again...
      |    {{/inline}}
      |    {{> myPartial}}
      |  {{/each}}
      |{{/name}}""".stripMargin

Template:
     Text(Testing...)
     Whitespace ||
     BlockHelper name (PathHelper(name))
        Whitespace |\n| clipped: |  |
        BlockHelper each (EachHelper(true))
           Whitespace || clipped: |    |
           InlinePartialTag(3)
              Whitespace |      | clipped: ||
              Text(Bar!     Again...)
              Whitespace |\n| clipped: |    |
           --> (end Inline partial)
           Whitespace || clipped: |    |
           HelperTag myPartial (PartialHelper(myPartial,Template:,true,false))
           Whitespace |  | clipped: ||
        --> (end BlockHelper)
        Whitespace |\n|
     --> (end BlockHelper)


 */
