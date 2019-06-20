package co.blocke.scalabars

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

  val t =
    """{{#* inline "nombre"}}
      |A
      |  B -- {{name}}
      |C
      |{{/inline}}
      |My name is:
      |{{>nombre}}
      |  Say it loud!""".stripMargin

  println(sb.compile(t)(json))

}
