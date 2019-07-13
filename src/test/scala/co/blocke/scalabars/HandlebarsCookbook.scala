package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class HandlebarsCookbook() extends FunSpec with Matchers {

  val sb = Scalabars()

  describe("-------------------------\n:  Handlebars Cookbook  :\n-------------------------") {
    describe("Blocks") {
      it("Block For Loop") {
        val json = org.json4s.native.JsonMethods.parse("""
                                                         |{
                                                         |  "foo": [
                                                         |    1,
                                                         |    3,
                                                         |    5
                                                         |  ]
                                                         |}
          """.stripMargin)
        val t = "{{#foo}}Ya!{{/foo}}"
        sb.compile(t)(json) should be("Ya!Ya!Ya!")
      }
      describe("Block for True") {
        it("Render inner for object") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": {
                                                           |    "bar": null,
                                                           |    "moo": null
                                                           |  }
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("Ya!")
        }
        it("Render inner for string") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": "test"
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("Ya!")
        }
        it("Render inner for number") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": 1
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("Ya!")
        }
        it("Render for 0") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": 0
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("Ya!")
        }
        it("Render for empty string") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": ""
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("Ya!")
        }
        it("Render for true") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": true
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("Ya!")
        }
      }
      describe("Block for False") {
        it("No render for false") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": false
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("")
        }
        it("No render for null or undefined") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": null
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("")
        }
        it("No render for empty array") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": []
                                                           |}
            """.stripMargin)
          val t = "{{#foo}}Ya!{{/foo}}"
          sb.compile(t)(json) should be("")
        }
      }
      describe("Context Switching") {
        it("Switch context for inner block") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": {
                                                           |    "foo": "Hello",
                                                           |    "bar": "World"
                                                           |  },
                                                           |  "bar": "OK"
                                                           |}
            """.stripMargin)
          val t = """{{foo}},{{bar}}
                    |{{#foo}}{{foo}},{{bar}}{{/foo}}""".stripMargin
          sb.compile(t)(json) should be("""[object Object],OK
                                          |Hello,World""".stripMargin)
        }
        it("Lookup deeper value") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": {
                                                           |    "bar": {
                                                           |      "moo": "MOO!"
                                                           |    }
                                                           |  }
                                                           |}
            """.stripMargin)
          val t = """{{#foo}}{{#bar}}{{moo}}{{/bar}}{{/foo}}"""
          sb.compile(t)(json) should be("MOO!")
        }
        it("Loop then context switched") {
          val json = org.json4s.native.JsonMethods.parse("""
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
                                                           |}
            """.stripMargin)
          val t = """{{#foo}}{{bar}}{{/foo}}"""
          sb.compile(t)(json) should be("Yes, hello world.")
        }
      }
    }
    describe("Whitespace Control") {
      it("Line change of standalone tags will not be removed") {
        val json = org.json4s.native.JsonMethods.parse("""
                                                         |{
                                                         |  "foo": 1
                                                         |}
          """.stripMargin)
        val t = """Line 1
                  |{{foo}}
                  |Line 3
                  |  {{foo}}
                  |Line 5
                  |     {{foo}}
                  |Line 7""".stripMargin
        sb.compile(t)(json) should be("""Line 1
                                        |1
                                        |Line 3
                                        |  1
                                        |Line 5
                                        |     1
                                        |Line 7""".stripMargin)
      }
      it("Line change of standalone block tags and {{else}} wil be removed") {
        val json = org.json4s.native.JsonMethods.parse("""
                                                         |{
                                                         |  "foo": 1
                                                         |}
          """.stripMargin)
        val t = """Line 1
                  |{{foo~}}
                  |Line 3
                  |  {{~foo~}}
                  |Line 5
                  |    {{~foo}}
                  |Line 7""".stripMargin
        sb.compile(t)(json) should be("""Line 1
                                        |1Line 31Line 51
                                        |Line 7""".stripMargin)
      }
    }
    describe("Comments") {
      describe("Inline") {
        it("Sample 1") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": "OK"
                                                           |}
            """.stripMargin)
          val t = """{{#foo}}Ya!{{! ignored this comment}}{{/foo}}"""
          sb.compile(t)(json) should be("""Ya!""")
        }
        it("Sample 2") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": true
                                                           |}
            """.stripMargin)
          val t =
            """{{#foo}}Yes{{!-- foo is true --}}{{/foo}}
              |{{^foo}}No{{! foo is false}}{{/foo}}""".stripMargin
          sb.compile(t)(json) should be("Yes\n")
        }
        it("Sample 3") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": true
                                                           |}
            """.stripMargin)
          val t = """Comment example: {{! comment with }} is not ok }}"""
          sb.compile(t)(json) should be("""Comment example:  is not ok }}""")
        }
        it("Sample 4 (with ws)") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": true
                                                           |}
                                                         """.stripMargin)
          val t = """{{#foo}}Ya! {{! ignored this comment}}{{/foo}}Done"""
          sb.compile(t)(json) should be("Ya! Done")
        }
      }
      describe("Block") {
        it("Sample 1") {
          val json = org.json4s.native.JsonMethods.parse("""
                                                           |{
                                                           |  "foo": "OK"
                                                           |}
                                                         """.stripMargin)
          val t =
            """{{#foo}}Ya!{{! ignored this 
              |comment}}
              |boing!{{/foo}}""".stripMargin
          sb.compile(t)(json) should be("Ya!\nboing!")
        }
      }
    }
  }
}
