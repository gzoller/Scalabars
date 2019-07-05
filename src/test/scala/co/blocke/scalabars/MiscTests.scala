package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }
import model._

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
      val t =
        """{{#each foo}}
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
        val t =
          """{{#each children as |child|}}
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
        val t =
          """{{#each children as |child id isFirst|}}
            |  {{#> childEntry}}
            |    {{id}} - {{child.value}} {{#isFirst}}First!{{/isFirst}}
            |  {{/childEntry}}
            |{{/each}}""".stripMargin
        // Strange '+' on this line to preserve leading space on "boat ", which is required for the test to work
        sb.compile(t)(json) should be("""Kid:     0 - car First!
                                        |Kid:     1 - boat""".stripMargin + " \n")
        val t2 =
          """{{#each children as |child id isFirst isLast|}}
            |  {{#> childEntry}}
            |    {{id}} - {{child.value}} {{#isFirst}}First!{{/isFirst}}{{#isLast}}Last!{{/isLast}}
            |  {{/childEntry}}
            |{{/each}}""".stripMargin
        sb.compile(t2)(json) should be("""Kid:     0 - car First!
                                         |Kid:     1 - boat Last!
                                         |""".stripMargin)
      }
      it("Object block parameter (each helper)") {
        val json = org.json4s.native.JsonMethods.parse("""{
                                                         |  "children": {
                                                         |    "name":"toy"
                                                         |    "value":"boat"
                                                         |  }
                                                         |}""".stripMargin)
        val t =
          """{{#each children as |thing key|}}
            |  {{#> childEntry}}
            |    {{key}} - {{thing}}
            |  {{/childEntry}}
            |{{/each}}""".stripMargin
        sb.compile(t)(json) should be("""Kid:     name - toy
                                        |Kid:     value - boat
                                        |""".stripMargin)
      }

    }
  }
}
