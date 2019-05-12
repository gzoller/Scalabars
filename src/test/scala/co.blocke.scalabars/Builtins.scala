package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Builtins() extends FunSpec with Matchers {

  val sb = Scalabars()
  val c = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List(Desc("cool"), Desc("wicked")), Person("Mike", 32))
  val c2 = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List.empty[Desc], Person("Mike", 32))
  val c3 = Magic("Mike", Map.empty[String, Int])
  val objData = Map("object" -> Map("one" -> 1, "two" -> 2))
  val listData = Map("collection" -> List("Professor Farnsworth", "Fry", "Bender"))

  describe("---------------------\n:  Builtin Helpers  :\n---------------------") {
    it("each") {
      sb.compile("""Hello, {{#each A}}Is this {{heavy}}?{{/each}} End!""").render(c) should be("Hello, Is this cool?Is this wicked? End!")
      sb.compile("""Hello, {{#each A}}Is this {{heavy}}?{{/each}} End!""").render(c2) should be("Hello,  End!")
      sb.compile("""Hello, {{#each A}}here{{else}}missing{{/each}} End!""").render(c2) should be("Hello, missing End!")
    }
    it("if") {
      sb.compile("""Hello, {{#if A}}here{{else}}missing{{/if}} End!""").render(c) should be("Hello, here End!")
      sb.compile("""Hello, {{#if bogus}}here{{else}}missing{{/if}} End!""").render(c) should be("Hello, missing End!")
      sb.compile("""Hello, {{#if stuff}}here{{else}}missing{{/if}} End!""").render(c3) should be("Hello, missing End!")
    }
    it("if compound else") {
      sb.compile("""Hello, {{#if bogus}}here{{else if A}}foo{{else}}missing{{/if}} End!""").render(c) should be("Hello, foo End!")
      sb.compile("""Hello, {{#if bogus}}here{{else if X}}foo{{else}}missing{{/if}} End!""").render(c) should be("Hello, missing End!")
    }
    it("with") {
      sb.compile("""Hello, {{#with player}}Is this {{name}} or {{age}}?{{/with}}!""").render(c) should be("Hello, Is this Mike or 32?!")
      sb.compile("""Hello, {{#with bogus}}here{{else}}missing{{/with}}!""").render(c2) should be("Hello, missing!")
      sb.compile("""Hello, {{#with stuff}}here{{else}}missing{{/with}}!""").render(c3) should be("Hello, missing!")
    }
    it("eq") {
      sb.compile("""Hello, {{#eq aNum 15}}here{{else}}missing{{/eq}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#eq name "Greg"}}here{{else}}missing{{/eq}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#eq "wicked" A.[1].heavy}}here{{else}}missing{{/eq}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#eq "nope" A.[1].heavy}}here{{else}}missing{{/eq}}!""").render(c) should be("Hello, missing!")
    }
    it("ne") {
      sb.compile("""Hello, {{#ne aNum 5}}here{{else}}missing{{/ne}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#ne name "Greg"}}here{{else}}missing{{/ne}}!""").render(c) should be("Hello, missing!")
      sb.compile("""Hello, {{#ne name A.[1].heavy}}here{{else}}missing{{/ne}}!""").render(c) should be("Hello, here!")
    }
    it("and") {
      sb.compile("""Hello, {{#and aNum true name}}here{{else}}missing{{/and}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#and aNum false name}}here{{else}}missing{{/and}}!""").render(c) should be("Hello, missing!")
      sb.compile("""Hello, {{#and aNum true bogus}}here{{else}}missing{{/and}}!""").render(c) should be("Hello, missing!")
    }
    it("or") {
      sb.compile("""Hello, {{#or aNum true name}}here{{else}}missing{{/or}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#or nope false bogus}}here{{else}}missing{{/or}}!""").render(c) should be("Hello, missing!")
    }
    it("unless") {
      sb.compile("""Hello, {{#unless A}}here{{else}}missing{{/unless}} End!""").render(c) should be("Hello, missing End!")
      sb.compile("""Hello, {{#unless bogus}}here{{else}}missing{{/unless}} End!""").render(c) should be("Hello, here End!")
      sb.compile("""Hello, {{#unless A}}here{{/unless}} End!""").render(c) should be("Hello,  End!")
      sb.compile("""Hello, {{#unless bogus}}here{{/unless}} End!""").render(c) should be("Hello, here End!")
    }
    it("eachIndex") {
      sb.compile("""{{#eachIndex collection}}{{this}} is {{index}} {{/eachIndex}}""").render(listData) should be("Professor Farnsworth is 0 Fry is 1 Bender is 2 ")
    }
    it("eachProperty") {
      sb.compile("""{{#eachProperty object}}{{key}} - {{value}} {{/eachProperty}}""").render(objData) should be("one - 1 two - 2 ")
    }
    it("contains") {
      sb.compile("""{{#contains object "two"}}yep{{else}}nope{{/contains}}""").render(objData) should be("yep")
      sb.compile("""{{#contains object "bogus"}}yep{{else}}nope{{/contains}}""").render(objData) should be("nope")
      sb.compile("""{{#contains collection "Fry"}}yep{{else}}nope{{/contains}}""").render(listData) should be("yep")
      sb.compile("""{{#contains collection "bogus"}}yep{{else}}nope{{/contains}}""").render(listData) should be("nope")
    }
    it("join") {
      sb.compile("""{{{join collection " & "}}}""").render(listData) should be("Professor Farnsworth & Fry & Bender")
    }
    it("length") {
      sb.compile("""{{length object}}""").render(objData) should be("2")
      sb.compile("""{{length collection}}""").render(listData) should be("3")
      sb.compile("""{{length bogus}}""").render(listData) should be("0")
    }
    it("lengthEquals") {
      sb.compile("""{{#lengthEquals object 2}}yep{{else}}nope{{/lengthEquals}}""").render(objData) should be("yep")
      sb.compile("""{{#lengthEquals object 3}}yep{{else}}nope{{/lengthEquals}}""").render(objData) should be("nope")
      sb.compile("""{{#lengthEquals collection 3}}yep{{else}}nope{{/lengthEquals}}""").render(listData) should be("yep")
      sb.compile("""{{#lengthEquals collection 0}}yep{{else}}nope{{/lengthEquals}}""").render(listData) should be("nope")
    }
    it("sortEach") {
      sb.compile("""{{#sortEach collection}}Foo: {{this}} {{/sortEach}}""").render(listData) should be("Foo: Bender Foo: Fry Foo: Professor Farnsworth ")
    }
    it("withBefore") {
      sb.compile("""{{#withBefore collection 1}}before: {{this}} {{/withBefore}}""").render(listData) should be("before: Professor Farnsworth before: Fry ")
    }
    it("withAfter") {
      sb.compile("""{{#withAfter collection 1}}after: {{this}} {{/withAfter}}""").render(listData) should be("after: Fry after: Bender ")
    }
    it("withFirst") {
      sb.compile("""Hello, {{#withFirst A}}thing {{this.heavy}}{{/withFirst}} End!""").render(c) should be("Hello, thing cool End!")
    }
    it("withLast") {
      sb.compile("""Hello, {{#withLast A}}thing {{this.heavy}}{{/withLast}} End!""").render(c) should be("Hello, thing wicked End!")
    }
  }
}