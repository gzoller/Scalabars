package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Builtins() extends FunSpec with Matchers {

  val sb = Scalabars()
  val c = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List(Desc("cool"), Desc("wicked")), Person("Mike", 32))
  val c2 = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List.empty[Desc], Person("Mike", 32))

  describe("---------------------\n:  Builtin Helpers  :\n---------------------") {
    it("each") {
      sb.compile("""Hello, {{#each A}}Is this {{heavy}}?{{/each}} End!""").render(c) should be("Hello, Is this cool?Is this wicked? End!")
      sb.compile("""Hello, {{#each A}}Is this {{heavy}}?{{/each}} End!""").render(c2) should be("Hello,  End!")
      sb.compile("""Hello, {{#each A}}here{{else}}missing{{/each}} End!""").render(c2) should be("Hello, missing End!")
    }
    it("if") {
      sb.compile("""Hello, {{#if A}}here{{else}}missing{{/if}} End!""").render(c) should be("Hello, here End!")
      sb.compile("""Hello, {{#if bogus}}here{{else}}missing{{/if}} End!""").render(c) should be("Hello, missing End!")
    }
    it("if compound else") {
      sb.compile("""Hello, {{#if bogus}}here{{else if A}}foo{{else}}missing{{/if}} End!""").render(c) should be("Hello, foo End!")
      sb.compile("""Hello, {{#if bogus}}here{{else if X}}foo{{else}}missing{{/if}} End!""").render(c) should be("Hello, missing End!")
    }
    it("with") {
      sb.compile("""Hello, {{#with player}}Is this {{name}} or {{age}}?{{/with}}!""").render(c) should be("Hello, Is this Mike or 32?!")
      sb.compile("""Hello, {{#with bogus}}here{{else}}missing{{/with}}!""").render(c2) should be("Hello, missing!")
    }
    it("eq") {
      sb.compile("""Hello, {{#eq aNum 15}}here{{else}}missing{{/eq}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#eq name "Greg"}}here{{else}}missing{{/eq}}!""").render(c) should be("Hello, here!")
      sb.compile("""Hello, {{#eq "wicked" A.[1].heavy}}here{{else}}missing{{/eq}}!""").render(c) should be("Hello, here!")
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
  }
}