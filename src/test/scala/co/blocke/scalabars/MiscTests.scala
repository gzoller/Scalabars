package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class MiscTests() extends FunSpec with Matchers {

  val sb = Scalabars().registerPartial("childEntry", "Kid: {{>@partial-block}}")
  val json = org.json4s.native.JsonMethods.parse("""
                                                   |{
                                                   |  "name": "Greg",
                                                   |  "age": 53,
                                                   |  "ok": true,
                                                   |  "html": "<b>Big</b>",
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

  describe("----------------\n:  Misc Tests  :\n----------------") {
    it("Raw block") {
      sb.compile("""{{{{raw}}}}This is a {{great}} test!{{{{/raw}}}}""")(json) should be("This is a {{great}} test!")
    }
    it("Escaping with {{{ }}}") {
      sb.compile("""Hello {{{html}}}""")(json) should be("Hello <b>Big</b>")
    }
    it("Escaping with {{& }}}") {
      sb.compile("""Hello {{&html}}""")(json) should be("Hello <b>Big</b>")
    }
    it("No escaping") {
      sb.compile("""Hello {{html}}""")(json) should be("Hello &lt;b&gt;Big&lt;/b&gt;")
    }
    it("Parent Context 1 (Handlebars Cookbook)") {
      val json = org.json4s.native.JsonMethods.parse("""{
                                                       |  "foo": [
                                                       |    {
                                                       |      "bar": "Yes!"
                                                       |    }
                                                       |  ],
                                                       |  "bar": "Another World"
                                                       |}""".stripMargin)
      sb.compile("{{#each foo}}{{bar}}, {{../bar}}{{/each}}")(json) should be("Yes!, Another World")
    }
    it("Parent Context 2 (Handlebars Cookbook)") {
      val json = org.json4s.native.JsonMethods.parse("""{
                                                       |  "foo": {
                                                       |    "0": {
                                                       |      "bar": {
                                                       |        "0": "ABC",
                                                       |        "1": "DEF",
                                                       |        "qoo": "GHI"
                                                       |      },
                                                       |      "qoo": "STU"
                                                       |    },
                                                       |    "1": {
                                                       |      "bar": {
                                                       |        "0": "V",
                                                       |        "1": "W",
                                                       |        "qoo": "XYZ"
                                                       |      },
                                                       |      "qoo": "YO!"
                                                       |    }
                                                       |  },
                                                       |  "qoo": "YA!"
                                                       |}""".stripMargin)
      val t = """{{#each foo}}
                |  {{#each bar}}
                |    {{.}},{{../qoo}},{{../../qoo}}
                |  {{/each}}
                |{{/each}}""".stripMargin
      sb.compile(t)(json) should be("""    ABC,STU,YA!
                                      |    DEF,STU,YA!
                                      |    GHI,STU,YA!
                                      |    V,YO!,YA!
                                      |    W,YO!,YA!
                                      |    XYZ,YO!,YA!
                                      |""".stripMargin)
    }
    describe("Child context and block parameters") {
      it("Simple (single) block parameter and context") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "children": [{
                                                         |    "value":"car"
                                                         |  },{
                                                         |    "value":"boat"
                                                         |  }]
                                                         |}""".stripMargin)
        val t = """{{#each children as |child|}}
                  |  {{#> childEntry}}
                  |    {{child.value}}
                  |  {{/childEntry}}
                  |{{/each}}""".stripMargin
        sb.compile(t)(json) should be("""Kid:     car
                                        |Kid:     boat
                                        |""".stripMargin)
      }
      it("Multiple block parameters (each helper)") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "children": [{
                                                         |    "value":"car"
                                                         |  },{
                                                         |    "value":"boat"
                                                         |  }]
                                                         |}""".stripMargin)
        val t = """{{#each children as |child id isFirst|}}
                  |  {{#> childEntry}}
                  |    {{id}} - {{child.value}} {{#isFirst}}First!{{/isFirst}}
                  |  {{/childEntry}}
                  |{{/each}}""".stripMargin
        // Strange '+' on this line to preserve leading space on "boat ", which is required for the test to work
        sb.compile(t)(json) should be("""Kid:     0 - car First!
                                        |Kid:     1 - boat""".stripMargin + " \n")
      }
      it("Object block parameter (each helper)") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "children": {
                                                         |    "name":"toy"
                                                         |    "value":"boat"
                                                         |  }
                                                         |}""".stripMargin)
        val t = """{{#each children as |thing key|}}
                  |  {{#> childEntry}}
                  |    {{key}} - {{thing}}
                  |  {{/childEntry}}
                  |{{/each}}""".stripMargin
        sb.compile(t)(json) should be("""Kid:     name - toy
                                        |Kid:     value - boat
                                        |""".stripMargin)
      }
    }
    describe("--------------\n:  Coverage  :\n--------------") {
      it("Scalabars") {
        sb.toString should be(
          "Scalabars(helpers=[lookup,lengthEquals,any,empty,join,url,if,withLookup,sortEach,else,withTake,or,each,last,raw,withDrop,default,unless,with,first,include,length,withFirst,withLast,contains,ne,eq,and,markdown])")
      }
      it("PathParser") {
        val t = """{{/foo/#bogus}}"""
        the[BarsException] thrownBy sb.compile(t)(json) should have message ("Template parsing failed: Parsed.Failure(Position 1:8, found \"#bogus}}\")")
      }
      it("HandlebarsParser") {
        val t = """This is a test  {  foo {{"""
        sb.compile(t).compiled.head.asInstanceOf[model.renderables.Text].s should be("This is a test  {  foo")
        val t2 = """This is a test"""
        sb.compile(t2).compiled.head.asInstanceOf[model.renderables.Text].s should be("This is a test")
      }
      it("UrlHelper") {
        the[BarsException] thrownBy sb.compile("""This is my {{# with name}}Hey {{url "http://www.yahoo.com"}}{{/with}}""")(json) should have message ("UrlHelper must be used within an object context")
      }
    }
  }
}
