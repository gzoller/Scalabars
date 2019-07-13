package co.blocke.scalabars

import org.json4s._
import org.scalatest.{ FunSpec, Matchers }
import model._

class StockHelpers() extends FunSpec with Matchers {

  val sb = Scalabars()

  val c = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List(Desc("cool"), Desc("wicked")), Person("Mike", 32))
  val c2 = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List.empty[Desc], Person("Mike", 32))
  val c3 = Magic("Mike", Map.empty[String, Int])
  val objData = Map("object" -> Map("one" -> 1, "two" -> 2), "mt" -> Map.empty[String, Int])
  val listData =
    Map("collection" -> List("Professor Farnsworth", "Fry", "Bender"), "mt" -> List.empty[String])

  describe("-------------------------\n:  Handlebars Cookbook  :\n-------------------------") {
    it("Simple Variable") {
      val data = org.json4s.native.JsonMethods.parse("""{
                                                       |  "foo":"Hello",
                                                       |  "true": "World",
                                                       |  "false": "Earth",
                                                       |  "undefined": "Word",
                                                       |  "ok": "Good"
                                                       |}
        """.stripMargin)
      val data2 = org.json4s.native.JsonMethods.parse("""["Hello","World"]""")

      sb.compile("{{foo}}")(data) should equal("Hello")
      sb.compile("{{bogus}}")(data) should equal("")
      sb.compile("{{true}}")(data) should equal("World")
      sb.compile("{{false}}")(data) should equal("Earth")
      sb.compile("{{undefined}}")(data) should equal("Word")
      sb.compile("{{1}}")(data2) should equal("World")
      sb.compile("{{ ok }}")(data) should equal("Good")
      sb.compile("{{ \nok\n\n }}")(data) should equal("Good")
    }
    it("@../index (path data variable lookup) must work") {
      val json = org.json4s.native.JsonMethods.parse("""{
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
      val t = """{{#each stuff}}
                |{{#each this}}
                |  {{@../index}}  {{this.name}}
                |{{/each}}
                |{{/each}}""".stripMargin
      sb.compile(t)(json) should be("""  0  Greg
                                      |  0  Lili
                                      |  1  Mike
                                      |  1  Sally
                                      |""".stripMargin)
    }
  }
  describe("---------------------------\n:  Stock Builtin Helpers  :\n---------------------------") {
    it("each") {
      sb.compile("""Hello, {{#each A}}Is this {{heavy}}?{{/each}} End!""")(c) should be("Hello, Is this cool?Is this wicked? End!")
      sb.compile("""Hello, {{#each A}}Is this {{heavy}}?{{/each}} End!""")(c2) should be("Hello,  End!")
      sb.compile("""Hello, {{#each A}}here{{else}}missing{{/each}} End!""")(c2) should be("Hello, missing End!")

      // With @index var
      sb.compile("""{{#each collection}}{{this}} is {{@index}} {{else}}nope{{/each}}""")(listData) should be(
        "Professor Farnsworth is 0 Fry is 1 Bender is 2 ")

      // With @first and @last var
      sb.compile("""{{#each collection}}{{this}} is {{@index}} {{#if @first}}First!{{else if @last}}Last!{{else}}(normal){{/if}}{{/each}}""")(
        listData) should be("Professor Farnsworth is 0 First!Fry is 1 (normal)Bender is 2 Last!")

      // With @key var (objects)
      sb.compile("""{{#each object}}{{@key}} - {{this}} {{else}}nope{{/each}}""")(objData) should be("one - 1 two - 2 ")

      // From Handlebars Cookbook
      val json = org.json4s.native.JsonMethods.parse("""{
                                                       |  "foo": [
                                                       |    "Hello",
                                                       |    "World"
                                                       |  ]
                                                       |}""".stripMargin)
      sb.compile("""{{#each foo}}{{.}}!{{/each}}""")(json) should equal("Hello!World!")
      sb.compile("""{{#each foo}}
                   |{{@index}}: {{.}}!
                   |{{/each}}""".stripMargin)(json) should equal("""0: Hello!
                                                                   |1: World!
                                                                   |""".stripMargin)
      val json2 = org.json4s.native.JsonMethods.parse("""{
                                                        |  "foo": {
                                                        |    "key": "Hello",
                                                        |    "key2": "World"
                                                        |  }
                                                        |}""".stripMargin)
      sb.compile("""{{#each foo}}{{.}}!{{/each}}""")(json2) should equal("Hello!World!")
      sb.compile("""{{#each foo}}
                   |{{@key}}: {{.}}!
                   |{{/each}}""".stripMargin)(json2) should equal("""key: Hello!
                                                                    |key2: World!
                                                                    |""".stripMargin)

      val json3 = org.json4s.native.JsonMethods.parse("""{
                                                        |  "foo": {
                                                        |    "first": "Hello",
                                                        |    "second": "World"
                                                        |  },
                                                        |  "title":"Values"
                                                        |}""".stripMargin)
      sb.compile("""{{#each foo}}
                   |{{../title}}:{{.}}!
                   |{{/each}}""".stripMargin)(json3) should equal("""Values:Hello!
                                                                    |Values:World!
                                                                    |""".stripMargin)
    }
    it("if") {
      sb.compile("""Hello, {{#if A}}here{{else}}missing{{/if}} End!""")(c) should be("Hello, here End!")
      sb.compile("""Hello, {{#if isOK}}here{{else}}missing{{/if}} End!""")(c) should be("Hello, missing End!")
      sb.compile("""Hello, {{#if bogus}}here{{else}}missing{{/if}} End!""")(c) should be("Hello, missing End!")
      sb.compile("""Hello, {{#if stuff}}here{{else}}missing{{/if}} End!""")(c3) should be("Hello, missing End!")

      // From Handlebars Cookbook
      val data = org.json4s.native.JsonMethods.parse("""{
                                                       |"foo": true,
                                                       |"foo2":"String",
                                                       |"foo3":[1],
                                                       |"foo4":1,
                                                       |"foo5":-1,
                                                       |"foo6":null,
                                                       |"foo7":false,
                                                       |"foo8":"",
                                                       |"foo9":[],
                                                       |"foo10":0
                                                       |}""".stripMargin)
      sb.compile("""{{#if foo}}YES{{else}}no{{/if}}""")(data) should equal("YES")
      sb.compile("""{{#if foo2}}YES{{else}}no{{/if}}""")(data) should equal("YES")
      sb.compile("""{{#if foo3}}YES{{else}}no{{/if}}""")(data) should equal("YES")
      sb.compile("""{{#if foo4}}YES{{else}}no{{/if}}""")(data) should equal("YES")
      sb.compile("""{{#if foo5}}YES{{else}}no{{/if}}""")(data) should equal("YES")
      sb.compile("""{{#if foo6}}YES{{else}}no{{/if}}""")(data) should equal("YES")
      sb.compile("""{{#if foo7}}YES{{else}}no{{/if}}""")(data) should equal("no")
      sb.compile("""{{#if foo8}}YES{{else}}no{{/if}}""")(data) should equal("YES")
      sb.compile("""{{#if foo9}}YES{{else}}no{{/if}}""")(data) should equal("no")
      sb.compile("""{{#if foo9}}YES{{^}}no{{/if}}""")(data) should equal("no")
      sb.compile("""{{#if foo10}}YES{{else}}no{{/if}}""")(data) should equal("YES")
    }
    it("if compound else") {
      sb.compile("""Hello, {{#if bogus}}here{{else if A}}foo{{else}}missing{{/if}} End!""")(c) should be("Hello, foo End!")
      sb.compile("""Hello, {{#if bogus}}here{{else if X}}foo{{else}}missing{{/if}} End!""")(c) should be("Hello, missing End!")
      sb.compile("""Hello, {{#if bogus}}here{{else if X}}foo{{^}}missing{{/if}} End!""")(c) should be("Hello, missing End!")
    }
    it("with") {
      sb.compile("""Hello, {{#with player}}Is this {{name}} or {{age}}?{{/with}}!""")(c) should be("Hello, Is this Mike or 32?!")
      sb.compile("""Hello, {{#with bogus}}here{{else}}missing{{/with}}!""")(c2) should be("Hello, missing!")
      sb.compile("""Hello, {{#with stuff}}here{{else}}missing{{/with}}!""")(c3) should be("Hello, missing!")

      // From Handlebars Cookbook
      val data = org.json4s.native.JsonMethods.parse("""{
                                                       |  "foo": {"bar":"Yes!"}
                                                       |}
        """.stripMargin)
      sb.compile("{{#with foo}}{{bar}}{{/with}}")(data) should equal("Yes!")

      val data2 = org.json4s.native.JsonMethods.parse("""{
                                                        |  "foo": {
                                                        |    "bar": {
                                                        |      "hey": "Hello",
                                                        |      "ha": "world"
                                                        |    }
                                                        |  }
                                                        |}
        """.stripMargin)
      sb.compile("{{#with foo.bar}}{{hey}}, {{ha}}{{/with}}")(data2) should equal("Hello, world")

      val data3 = org.json4s.native.JsonMethods.parse("""{
                                                        |  "boom": "foo",
                                                        |  "foo": {
                                                        |    "bar": {
                                                        |      "moo": "Hello"
                                                        |    }
                                                        |  },
                                                        |  "bar": "world"
                                                        |}
        """.stripMargin)
      sb.compile("{{#with foo.bar}}{{moo}}, {{../bar}}{{/with}}")(data3) should equal("Hello, world")
      sb.compile("{{#with foo.bar as |x|}}{{moo}}, {{x.moo}}{{../bar}}{{/with}}")(data3) should equal("Hello, Helloworld")

      sb.compile("{{#with 0}}Current context:{{.}}{{/with}}")("") should equal("Current context:0")
      sb.compile("{{#with 1}}Current context:{{.}}{{/with}}")("") should equal("Current context:1")
      sb.compile("{{#with .}}Current context:{{.}}{{/with}}")(List.empty[String]) should equal("")
      sb.compile("{{#with false}}Current context:{{.}}{{/with}}")(List.empty[String]) should equal("")
      sb.compile("""{{#with true}}Current context:{{.}}{{/with}}""")(List.empty[String]) should equal("Current context:")

      sb.compile("""{{#with (lookup . "boom") as |x|}}Here {{x.bar.moo}}{{/with}}""")(data3) should be("Here Hello")
      sb.compile("""{{#with 0 as |x|}}Here {{x}}{{/with}}""")(data3) should be("Here 0")
    }
    it("unless") {
      sb.compile("""Hello, {{#unless A}}here{{else}}missing{{/unless}} End!""")(c) should be("Hello, missing End!")
      sb.compile("""Hello, {{#unless bogus}}here{{else}}missing{{/unless}} End!""")(c) should be("Hello, here End!")
      sb.compile("""Hello, {{#unless A}}here{{/unless}} End!""")(c) should be("Hello,  End!")
      sb.compile("""Hello, {{#unless bogus}}here{{/unless}} End!""")(c) should be("Hello, here End!")

      // From Handlebars Cookbook
      val data = org.json4s.native.JsonMethods.parse("""{
                                                       |"foo": true,
                                                       |"foo2":"String",
                                                       |"foo3":[1],
                                                       |"foo4":1,
                                                       |"foo5":-1,
                                                       |"foo6":null,
                                                       |"foo7":false,
                                                       |"foo8":"",
                                                       |"foo9":[]
                                                       |"foo10":0
                                                       |}""".stripMargin)
      sb.compile("""{{#unless foo}}YES{{else}}no{{/unless}}""")(data) should equal("no")
      sb.compile("""{{#unless foo2}}YES{{else}}no{{/unless}}""")(data) should equal("no")
      sb.compile("""{{#unless foo3}}YES{{else}}no{{/unless}}""")(data) should equal("no")
      sb.compile("""{{#unless foo4}}YES{{else}}no{{/unless}}""")(data) should equal("no")
      sb.compile("""{{#unless foo5}}YES{{else}}no{{/unless}}""")(data) should equal("no")
      sb.compile("""{{#unless foo6}}YES{{else}}no{{/unless}}""")(data) should equal("no")
      sb.compile("""{{#unless foo7}}YES{{else}}no{{/unless}}""")(data) should equal("YES")
      sb.compile("""{{#unless foo8}}YES{{else}}no{{/unless}}""")(data) should equal("no")
      sb.compile("""{{#unless foo9}}YES{{else}}no{{/unless}}""")(data) should equal("YES")
      sb.compile("""{{#unless foo10}}YES{{else}}no{{/unless}}""")(data) should equal("no")
    }
    it("lookup") {
      val t = sb.compile("""Hello, {{lookup collection @idx}}!""")
      val ctx: Context = t.contextFromObj(listData)
      val ctx2 = ctx.setData("idx", LongEvalResult(1))
      t.render(ctx2) should equal("Hello, Fry!")

      val t2 = sb.compile("""Hello, {{lookup object @item}}!""")
      val ctx3 = t.contextFromObj(objData)
      val ctx4 = ctx3.setData("item", StringEvalResult("one"))
      t2.render(ctx4) should equal("Hello, 1!")

      the[BarsException] thrownBy sb
        .compile("Foo {{lookup object nada}}")
        .render(ctx4) should have message "lookup helper failed to resolve with arguments: object and nada"

      // From Handlebars Cookbook
      val data = Stuff(List("Hello", "World"), Map("Hello" -> "first", "World" -> "second"))
      val tE = """{{#each foo}}
                 |{{lookup ../bar .}}:{{.}}!
                 |{{/each}}""".stripMargin
      sb.compile(tE)(data) should equal("""first:Hello!
                                          |second:World!
                                          |""".stripMargin)

      val data2 = Stuff2(Map("Hello" -> "first", "World" -> "second"), Map("first" -> 1, "second" -> 2), ">b<")
      val tO =
        """<b>here</b>
          |{{#each foo}}{{@key}}:{{.}} {{../thing}} => {{lookup ../bar .}}
          |{{/each}}""".stripMargin
      sb.compile(tO)(data2) should equal("""<b>here</b>
                                           |Hello:first &gt;b&lt; => 1
                                           |World:second &gt;b&lt; => 2
                                           |""".stripMargin)
    }
    it("helperMissing") {
      val sb2 = sb.registerHelper(
        "helperMissing",
        """function(arr, options) {
          |  return "nothing here";
          |}
        """.stripMargin
      )
      sb2.compile("This {{foo}}")(c) should be("This nothing here")
    }
    it("blockHelperMissing") {
      val sb2 = sb.registerHelper(
        "blockHelperMissing",
        """function(arr, options) {
          |  return "nothing here";
          |}
        """.stripMargin
      )
      sb2.compile("This {{#foo}}blah{{/foo}}")(c) should be("This nothing here")
    }
    it("log") {
      val tHandler = new TestHandler()
      val sbx = Scalabars(Some(JavaLogger(Some(tHandler))))
      sbx.compile("""Hello {{log "why"}}!""")(c) should be("Hello !")
      sbx.compile("""Hello {{log "why" name level="warn"}}!""")(c) should be("Hello !")
      sbx.compile("""Hello {{log "why" name level="error"}}!""")(c) should be("Hello !")
      sbx.compile("""Hello {{log "why" name level="info"}}!""")(c) should be("Hello !")
      sbx.compile("""Hello {{log "why" name level="debug"}}!""")(c) should be("Hello !")

      tHandler.buffer should be("""INFO: why
                                  |WARNING: why Greg
                                  |SEVERE: why Greg
                                  |INFO: why Greg
                                  |""".stripMargin)
    }
  }
}

class TestHandler() extends java.util.logging.Handler {
  private val strbuf = new StringBuilder()
  def close(): Unit = {}
  def flush(): Unit = {}
  def publish(record: java.util.logging.LogRecord): Unit = { strbuf.append(record.getLevel() + ": " + record.getMessage() + "\n") }
  def buffer: String = strbuf.toString
}
