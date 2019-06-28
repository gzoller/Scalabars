package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Partials() extends FunSpec with Matchers {

  val sb =
    Scalabars().registerPartial("myPartial", "{{name}}!").registerPartial("myPartialD", "{{item}}!")
  val json = org.json4s.native.JsonMethods.parse("""
                                                   |{
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
                                                   |  "which": "myPartial",
                                                   |  "numbers":[5,6,7,8],
                                                   |  "numberSet":[[5,7],[8,9]],
                                                   |  "player":{
                                                   |    "name": "David",
                                                   |    "age": 12
                                                   |  }
                                                   |}
    """.stripMargin)

  describe("--------------\n:  Partials  :\n--------------") {
    describe("Simple") {
      it("Unparameterized") {
        sb.compile("""Hello, {{> myPartial}}""")(json) should be("Hello, Greg!")
      }
      it("Dynamic Unparameterized") {
        sb.compile("""Hello, {{> (lookup . "which")}}""")(json) should be("Hello, Greg!")
      }
      it("With given context") {
        sb.compile("""Hello, {{> myPartial player}}""")(json) should be("Hello, David!")
      }
      it("With custom data") {
        sb.compile("""Hello, {{> myPartialD item=name}}""")(json) should be("Hello, Greg!")
      }
      it("explicitPartialContext mode") {
        sb.compile("""Hello, {{> myPartial player}}""", Map("explicitPartialContext" -> true))(json) should be(
          "Hello, !")
        sb.compile("""Hello, {{> myPartialD item=name}}""", Map("explicitPartialContext" -> true))(
          json) should be("Hello, Greg!")
      }
    }
    describe("From Handlebars Cookbook") {
      it("Current context") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "foo": "World",
                                                         |  "bar": {
                                                         |    "foo": "YA!"
                                                         |  }
                                                         |  }""".stripMargin)
        val sb2 = sb.registerPartial("world", "{{foo}}!").registerPartial("hello", "{{bar}}!")
        sb2.compile("{{>hello}}, {{> world}}\nFoo: {{#bar}}{{> world}}{{/bar}}")(json) should be(
          "[object Object]!, World!\nFoo: YA!!")
      }
      it("Given context") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "bar": {
                                                         |    "moo": "GOOD!"
                                                         |  }
                                                         |}""".stripMargin)
        val sb2 = sb.registerPartial("foo", "{{moo}}")
        sb2.compile("{{>foo bar}}")(json) should be("GOOD!")
      }
    }
    describe("Block") {
      it("Normal render (no block)") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "bar": {
                                                         |    "moo": "GOOD!"
                                                         |  }
                                                         |}""".stripMargin)
        val sb2 = sb.registerPartial("foo", "{{moo}}")
        sb2.compile("{{#>foo bar}}Boom{{/foo}}")(json) should be("GOOD!")
      }
      it("Fallback render") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "bar": {
                                                         |    "moo": "GOOD!"
                                                         |  }
                                                         |}""".stripMargin)
        val sb2 = sb.registerPartial("foo", "{{moo}}")
        sb2.compile("{{#>bogus bar}}Boom{{/bogus}}")(json) should be("Boom")
      }
      it("Block content as template parameter (@partial-block)") {
        val sb2 = sb.registerPartial(
          "layout",
          """Site Content
                                       |{{> @partial-block }}""".stripMargin)
        sb2.compile("""{{#> layout }}
                      |  My Content{{/layout}}""".stripMargin)(json) should be("""Site Content
                                                                                 |  My Content""".stripMargin)
      }
      describe("Inline") {
        it("Standard inline partial") {
          val sb2 = Scalabars()
          val t = """{{#*inline "myPartial"}}
                    |  My Content
                    |{{/inline}}
                    |{{#each numbers}}
                    |  {{> myPartial}}
                    |{{/each}}""".stripMargin

          sb2.compile(t)(json) should be("""    My Content
                                           |    My Content
                                           |    My Content
                                           |    My Content
                                           |""".stripMargin)
        }
        it("Block-specific inline partial (nested inline with same name, block-scoped)") {
          val sb2 = Scalabars() // just to clear everything out from other tests, just in case...
          val t =
            """Testing...{{#name}}
              |  {{#*inline "myPartial"}}
              |    Foo!
              |  {{/inline}}
              |  {{> myPartial}}
              |  {{#each ../interests}}
              |    {{#*inline "myPartial"}}
              |      Bar!
              |      Again...
              |    {{/inline}}
              |    {{> myPartial}}
              |    {{#with this}}
              |      Here {{>myPartial}}
              |    {{/with}}
              |  {{/each}}
              |{{/name}}""".stripMargin
          sb2.compile(t)(json) should be("""Testing...
                                           |      Foo!
                                           |          Bar!
                                           |          Again...
                                           |      Here       Bar!
                                           |      Again...
                                           |
                                           |          Bar!
                                           |          Again...
                                           |      Here       Bar!
                                           |      Again...
                                           |
                                           |""".stripMargin)
        }
        it("Inline partial inside a registerPartial()") {
          val sb2 = Scalabars().registerPartial(
            "outer",
            """Ok {{#* inline "inner"}}stuff{{/inline}}more {{>inner}}.""")
          sb2.compile("Here we go: {{>outer}}")(json) should be("Here we go: Ok more stuff.")
        }
      }
    }
  }
}
