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
        val t2 = """{{#each children as |child id isFirst isLast|}}
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
        the[BarsException] thrownBy sb.compile(t)(json) should have message "Template parsing failed: Parsed.Failure(Position 1:8, found \"#bogus}}\")"
      }
      it("HandlebarsParser") {
        val t = """This is a test  {  foo {{"""
        sb.compile(t).compiled.head.asInstanceOf[model.renderables.Text].s should be("This is a test  {  foo")
        val t2 = """This is a test"""
        sb.compile(t2).compiled.head.asInstanceOf[model.renderables.Text].s should be("This is a test")
        sb.compile("""This is {""").compiled.head.asInstanceOf[model.renderables.Text].s should be("This is {")
      }
      it("UrlHelper") {
        the[BarsException] thrownBy sb.compile("""This is my {{# with name}}Hey {{url "http://www.yahoo.com"}}{{/with}}""")(json) should have message "UrlHelper must be used within an object context"
      }
      it("Template") {
        sb.compile("foo{{this}}").toString should be("""Template:
                                                       |     Text(foo)
                                                       |     Whitespace ||
                                                       |     HelperTag this (PathHelper(this))
                                                       |       args: List()
                                                       |""".stripMargin)
        model.EmptyTemplate().render(model.Context.NotFound) should be("")
      }
      it("Options") {
        sb.compile("Foo{{this}}").compileOptions.toString should be(
          """Options:
            |   Hash = Map(noEscape -> false, strict -> false, preventIndent -> false, explicitPartialContext -> false)
            |   Context = None""".stripMargin)

        sb.compile("""{{#if "false"}}A{{else}}B{{/if}}""")(json) should be("B")
        sb.registerHelper("assignJS", """function(options){ return "Greetings "+ options.hash["bogus"]+"!"; }""")
          .compile("""Hello and {{assignJS tidal="wave"}}""")(json) should be("Hello and Greetings undefined!")
      }
      it("Args") {
        sb.compile("""{{#with 12.34}}Hi {{this}}{{/with}}""")(json) should be("Hi 12.34")
      }
      it("Context") {
        the[BarsException] thrownBy sb.compile("""{{../foo}}""")(json) should have message "Path cannot back up (..) beyond history: ../foo"
        the[BarsException] thrownBy sb.compile("""{{name.bogus}}""")(json) should have message "Illegal attempt to reference a field on a non-object: name.bogus"
        val c = model.Context.NotFound
        c.flatten() should be(model.Context.NotFound)
        the[BarsException] thrownBy sb.compile("""{{name.2}}""")(json) should have message "Can't index into a non-array in path: name.2"

        // Test other @data var types (e.g. Double, SafeString)
        val sb2 = sb.registerHelper(
          "jsEach",
          """
            |function(arr, options) {
            |  if(!arr || arr.length === 0) {
            |    return options.inverse();
            |  }
            |
            |  var data={};
            |  if( options.data ) {
            |    data = Handlebars.createFrame(options.data);
            |  }
            |
            |  var result = [];
            |  data.double = 12.345;
            |  data.safe = Handlebars.SafeString("I'm safe");
            |  for(var i=0; i<arr.length; i++) {
            |    if(data) {
            |      data.index = i;
            |    }
            |    result.push(options.fn(arr[i], {data: data}));
            |  }
            |
            |  return result.join('');
            |}""".stripMargin
        )

        sb2.compile("""{{#jsEach numbers}}Here {{@double}}{{@safe}}{{/jsEach}}""")(json) should be(
          "Here 12.345I'm safeHere 12.345I'm safeHere 12.345I'm safeHere 12.345I'm safe")

        sb.registerHelper("foo", HelperSample()).compile("""{{# foo}}This is a {{@safe}}{{@ctx}} test{{/foo}}""".stripMargin)(json) should be(
          "This is a I'm safe[object Object],[object Object] test")
      }
      it("Whitespace") {
        sb.compile("""  {{#hey}}
                     |Foo{{/hey}}
          """.stripMargin).compiled.head.toString should be("Whitespace || clipped: |  |")
      }
      it("InlinePartialTag") {
        val t =
          """{{#*inline 12.34}}
            |     Here {{@wow}}
            |{{/inline}}
            |{{> myPartial}}""".stripMargin
        the[BarsException] thrownBy sb.compile(t)(json) should have message ("Inline partial's argument must evaluate to a string")
        sb.compile(t).compiled.head.toString should be("""InlinePartialTag(5)
                                                         |    Whitespace |     | clipped: ||
                                                         |    Text(Here)
                                                         |    Whitespace | |
                                                         |    HelperTag @wow (PathHelper(@wow))
                                                         |  args: List()
                                                         |
                                                         |    Whitespace |\n| clipped: ||
                                                         |--> (end Inline partial)""".stripMargin)
      }

    }
    it("Raw") {
      sb.compile("""{{{{#name}}}}I'm {{this}}{{{{/name}}}}""")(json) should be("I'm {{this}}")
    }
    it("ExpressionHelper") {
      val json = org.json4s.native.JsonMethods.parse("""{
                                                       |  "index": 15,
                                                       |  "bogus": "nothing"
                                                       |}""".stripMargin)
      the[BarsException] thrownBy sb.compile("""Number: {{(lookup . "index") interests "[1].label"}}""")(json) should have message "Expression helpers must resolve to a String"
      the[BarsException] thrownBy sb.compile("""Number: {{(lookup . "bogus") interests "[1].label"}}""")(json) should have message "Expression eval to 'nothing' but no helper by this name is registered."
      the[BarsException] thrownBy sb.compile("""Number: {{>(lookup . "bogus") interests "[1].label"}}""")(json) should have message "Expression eval to 'nothing' but no partial by this name is registered."
    }
  }
}

case class HelperSample() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val data: Map[String, EvalResult[_]] = Map("safe" -> SafeStringEvalResult("I'm safe"), "ctx" -> options.context.lookup("interests"))
    options.fn(options.context, data)
  }
}
