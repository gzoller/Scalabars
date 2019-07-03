package co.blocke.scalabars

import org.json4s._
import org.scalatest.{ FunSpec, Matchers }
import model._

class AdditionalHelpers() extends FunSpec with Matchers {

  val sb = Scalabars()
  val c = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List(Desc("cool"), Desc("wicked")), Person("Mike", 32))
  val c2 = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List.empty[Desc], Person("Mike", 32))
  val c3 = Magic("Mike", Map.empty[String, Int])
  val objData = Map("object" -> Map("one" -> 1, "two" -> 2), "mt" -> Map.empty[String, Int])
  val listData =
    Map("collection" -> List("Professor Farnsworth", "Fry", "Bender"), "mt" -> List.empty[String])

  describe("---------------------------\n:  Extra Builtin Helpers  :\n---------------------------") {
    describe("Collections") {
      it("any") {
        sb.compile("""{{#any object}}yep{{else}}nope{{/any}}""")(objData) should be("yep")
        sb.compile("""{{#any collection}}yep{{else}}nope{{/any}}""")(listData) should be("yep")
        sb.compile("""{{#any bogus}}yep{{else}}nope{{/any}}""")(objData) should be("nope")
        sb.compile("""{{#any bogus}}yep{{else}}nope{{/any}}""")(listData) should be("nope")
      }
      it("contains") {
        sb.compile("""{{#contains object "two"}}yep{{else}}nope{{/contains}}""")(objData) should be("yep")
        sb.compile("""{{#contains object "bogus"}}yep{{else}}nope{{/contains}}""")(objData) should be("nope")
        sb.compile("""{{#contains collection "Fry"}}yep{{else}}nope{{/contains}}""")(listData) should be("yep")
        sb.compile("""{{#contains collection "bogus"}}yep{{else}}nope{{/contains}}""")(listData) should be("nope")
      }
      it("empty") {
        sb.compile("""{{#empty object}}yep{{else}}nope{{/empty}}""")(objData) should be("nope")
        sb.compile("""{{#empty collection}}yep{{else}}nope{{/empty}}""")(listData) should be("nope")
        sb.compile("""{{#empty mt}}yep{{else}}nope{{/empty}}""")(objData) should be("yep")
        sb.compile("""{{#empty mt}}yep{{else}}nope{{/empty}}""")(listData) should be("yep")
      }
      it("first") {
        sb.compile("""{{first collection}}""")(listData) should be("Professor Farnsworth")
        sb.compile("""{{first x}}""")(listData) should be("")
      }
      it("join") {
        sb.compile("""{{{join collection " & "}}}""")(listData) should be("Professor Farnsworth & Fry & Bender")
        sb.compile("""{{{join collection collection}}}""")(listData) should be("Professor FarnsworthFryBender")
        sb.compile("""{{{join object "."}}}""")(objData) should be("")
      }
      it("last") {
        sb.compile("""{{last collection}}""")(listData) should be("Bender")
        sb.compile("""{{last x}}""")(listData) should be("")
      }
      it("length") {
        sb.compile("""{{length object}}""")(objData) should be("2")
        sb.compile("""{{length collection}}""")(listData) should be("3")
        sb.compile("""{{length bogus}}""")(listData) should be("0")
      }
      it("lengthEquals") {
        sb.compile("""{{#lengthEquals object 2}}yep{{else}}nope{{/lengthEquals}}""")(objData) should be("yep")
        sb.compile("""{{#lengthEquals object 3}}yep{{else}}nope{{/lengthEquals}}""")(objData) should be("nope")
        sb.compile("""{{#lengthEquals collection 3}}yep{{else}}nope{{/lengthEquals}}""")(listData) should be("yep")
        sb.compile("""{{#lengthEquals collection 0}}yep{{else}}nope{{/lengthEquals}}""")(listData) should be("nope")
        sb.compile("""{{#lengthEquals name 2}}yep{{else}}nope{{/lengthEquals}}""")(c) should be("nope")
      }
      it("sortEach") {
        sb.compile("""{{#sortEach collection}}Foo: {{this}} {{/sortEach}}""")(listData) should be("Foo: Bender Foo: Fry Foo: Professor Farnsworth ")
        sb.compile("""{{#sortEach object}}Foo: {{this}} {{/sortEach}}""")(objData) should be("")
      }
      it("withDrop") {
        sb.compile("""{{#withDrop collection 1}}after: {{this}} {{/withDrop}}""")(listData) should be("after: Fry after: Bender ")
        sb.compile("""{{#withDrop object 1}}after: {{this}} {{else}}nope{{/withDrop}}""")(objData) should be("nope")
        sb.compile("""{{#withDrop collection true}}after: {{this}} {{else}}nope{{/withDrop}}""")(listData) should be("nope")
        sb.compile("""{{#withDrop collection true}}after: {{this}} {{/withDrop}}""")(listData) should be("")
      }
      it("withTake") {
        sb.compile("""{{#withTake collection 1}}before: {{this}} {{/withTake}}""")(listData) should be("before: Professor Farnsworth ")
        sb.compile("""{{#withTake object 1}}before: {{this}} {{else}}nope{{/withTake}}""")(listData) should be("nope")
        sb.compile("""{{#withTake collection true}}before: {{this}} {{else}}nope{{/withTake}}""")(listData) should be("nope")
        sb.compile("""{{#withTake collection true}}before: {{this}} {{/withTake}}""")(listData) should be("")
      }
      it("withFirst") {
        sb.compile("""Hello, {{#withFirst A}}thing {{this.heavy}}{{/withFirst}} End!""")(c) should be("Hello, thing cool End!")
        sb.compile("""Hello, {{#withFirst name}}thing {{else}}nope{{this.heavy}}{{/withFirst}} End!""")(c) should be("Hello, nope End!")
        sb.compile("""Hello, {{#withFirst A}}thing {{else}}nope{{this.heavy}}{{/withFirst}} End!""")(c2) should be("Hello, nope End!")
      }
      it("withLast") {
        sb.compile("""Hello, {{#withLast A}}thing {{this.heavy}}{{/withLast}} End!""")(c) should be("Hello, thing wicked End!")
        sb.compile("""Hello, {{#withLast bogus}}thing {{else}}nope{{this.heavy}}{{/withLast}} End!""")(c) should be("Hello, nope End!")
        sb.compile("""Hello, {{#withLast A}}thing {{else}}nope{{this.heavy}}{{/withLast}} End!""")(c2) should be("Hello, nope End!")
      }
      it("withLookup") {
        val t = sb.compile("""Hello, {{#withLookup A @idx}}say {{heavy}}{{/withLookup}}!""")
        val ctx = t.contextFromObj(c)
        val ctx2 = ctx.setData("idx", LongEvalResult(1))
        t.render(ctx2) should equal("Hello, say wicked!")
        val t2 = sb.compile("""Hello, {{#withLookup this @item}}say {{name}}{{/withLookup}}!""")
        val ctx3 = ctx2.setData("item", StringEvalResult("player"))
        t2.render(ctx3) should equal("Hello, say Mike!")
        sb.compile("Foo {{#withLookup object nada}}hi{{else}}bye{{/withLookup}}")
          .render(ctx3) should equal("Foo bye")
      }
    }
    describe("Misc") {
      it("url") {
        sb.compile("""This is my {{#url "http://www.yahoo.com"}}x = {{host}} - y = {{port}} - {{collection.[1]}}{{else}}nope{{/url}}""")(listData) should be(
          "This is my x = www.yahoo.com - y = -1 - Fry")
        sb.compile("""This is my {{#url "bogust123"}}x = {{host}} - y = {{port}} - {{collection.[1]}}{{else}}nope{{/url}}""")(listData) should be(
          "This is my nope")
      }
      it("markdown") {
        val t = """{{#markdown}}
                  |## Post of the day
                  |```scala
                  |val x = "foo"
                  |```
                  |Vestibulum posuere, {{name}} sed bibendum posuere
                  |### Section 1
                  |Pellentesque nulla {{player.age}}, volutpat vitae
                  |{{/markdown}}""".stripMargin
        sb.compile(t)(c) should equal("""<h2>Post of the day</h2>
                                        |<pre><code class="language-scala">val x = &quot;foo&quot;
                                        |</code></pre>
                                        |<p>Vestibulum posuere, Greg sed bibendum posuere</p>
                                        |<h3>Section 1</h3>
                                        |<p>Pellentesque nulla 32, volutpat vitae</p>
                                        |""".stripMargin)
      }
      it("default") {
        sb.compile("""Hello, {{default name "nobody"}}!""")(c) should be("Hello, Greg!")
        sb.compile("""Hello, {{default name2 "nobody"}}!""")(c) should be("Hello, nobody!")
      }
      it("include") {
        case class TestFileGetter() extends FileGetter {
          def retrieveFileAtPath(path: String): String = "Embed {{name}}"
        }
        val sb2 = sb.setFileGetter(TestFileGetter())
        sb2.compile("""Hello, {{include "/any/path" this}}!""")(c) should be("Hello, Embed Greg!")
      }
    }
    describe("Comparisons") {
      it("eq") {
        sb.compile("""Hello, {{#eq aNum 15}}here{{else}}missing{{/eq}}!""")(c) should be("Hello, here!")
        sb.compile("""Hello, {{#eq name "Greg"}}here{{else}}missing{{/eq}}!""")(c) should be("Hello, here!")
        sb.compile("""Hello, {{#eq "wicked" A.[1].heavy}}here{{else}}missing{{/eq}}!""")(c) should be("Hello, here!")
        sb.compile("""Hello, {{#eq "nope" A.[1].heavy}}here{{else}}missing{{/eq}}!""")(c) should be("Hello, missing!")
      }
      it("ne") {
        sb.compile("""Hello, {{#ne aNum 5}}here{{else}}missing{{/ne}}!""")(c) should be("Hello, here!")
        sb.compile("""Hello, {{#ne name "Greg"}}here{{else}}missing{{/ne}}!""")(c) should be("Hello, missing!")
        sb.compile("""Hello, {{#ne name A.[1].heavy}}here{{else}}missing{{/ne}}!""")(c) should be("Hello, here!")
      }
      it("and") {
        sb.compile("""Hello, {{#and aNum true name}}here{{else}}missing{{/and}}!""")(c) should be("Hello, here!")
        sb.compile("""Hello, {{#and aNum false name}}here{{else}}missing{{/and}}!""")(c) should be("Hello, missing!")
        sb.compile("""Hello, {{#and aNum true bogus}}here{{else}}missing{{/and}}!""")(c) should be("Hello, missing!")
      }
      it("or") {
        sb.compile("""Hello, {{#or aNum true name}}here{{else}}missing{{/or}}!""")(c) should be("Hello, here!")
        sb.compile("""Hello, {{#or nope false bogus}}here{{else}}missing{{/or}}!""")(c) should be("Hello, missing!")
      }
    }
  }
}
