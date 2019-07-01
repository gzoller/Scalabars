package co.blocke.scalabars

import model._

object Runme extends App {

  val sb = Scalabars()

  val json = org.json4s.native.JsonMethods.parse("""
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

  // TODO:  WS before {{> myPartial}} is empty!  It's "consumed" by #each's trailing ws, which almost
  // TODO:  has "\n  " in it!  This is getting missed in TagBuilder, and may be hard to fix there....
  // TODO:  May be a fix made in PostParse?

  val js = org.json4s.native.JsonMethods.parse("""
                                                 |{
                                                 |  "foo": [
                                                 |    {
                                                 |      "bar": "Yes, "
                                                 |    },
                                                 |    {
                                                 |      "bar": "hello "
                                                 |    },
                                                 |    {
                                                 |      "bar": "world."
                                                 |    }
                                                 |  ]
                                                 |}""".stripMargin)

  val t = """{{#foo}}{{bar}}{{/foo}}"""
  println(sb.compile(t)(js))

  println("-----")

}

/*
Synthetic::::::

BlockHelper each (EachHelper(true))
  args: List(PathArgument(foo))
  contents:
    Whitespace(,false)
    HelperTag bar (PathHelper(bar))
  args: List()

    Whitespace(,false)
--> (end BlockHelper)

Real Each :::::::

BlockHelper each (EachHelper(true))
       args: List()
       contents:
         Whitespace(,false)
         HelperTag bar (PathHelper(bar))
       args: List()

         Whitespace(,false)
     --> (end BlockHelper)


 */

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
