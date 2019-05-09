package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

case class Person(name: String, age: Int)
case class Desc(heavy: String)
case class Data(
    name:   String,
    msg:    String,
    aNum:   Int,
    isOK:   Boolean,
    small:  Long,
    A:      List[Desc],
    player: Person
)

class Interpreting extends FunSpec with Matchers {

  val sb = Scalabars()
    .registerHelper("foo", FooHelper())
    .registerHelper("twotags", TwoTagsHelper())
    .registerHelper("bar", BarHelper())
    .registerHelper("hash", HashHelper())
    .registerHelper("hashObj", HashObjHelper())
    .registerHelper("allTypes", AllTypesHelper())
    .registerHelper("raw", RawHelper())
    .registerHelper("context", ContextHelper())
    .registerHelper("noop", NoopHelper())
    .registerHelper("oneop", OneOpHelper())
    .registerHelper("strArg", StrArgHelper())

  /*
    .registerHelper("jsEach", """
                                |function(context, options) {
                                |  var ret = "";
                                |
                                |  for(var i=0, j=context.length; i<j; i++) {
                                |    ret = ret + options.fn(context[i]);
                                |  }
                                |
                                |  return ret;
                                |}""".stripMargin)
    .registerHelper(
      "jsIf",
      """
        |function(conditional, options) {
        |  if(conditional) {
        |    return options.fn(this);
        |  } else {
        |    return options.inverse(this);
        |  }
        |}""".stripMargin)
     */

  val c = Data("Greg", "<p>Yay!</p>", 15, false, 2L, List(Desc("cool"), Desc("wicked")), Person("Mike", 32))

  describe("--------------------------------------\n:  Handlebars Interpreting (Native)  :\n--------------------------------------") {
    describe("Single tag (non-block) interpreting") {
      it("Interprets 1-element path") {
        sb.compile("Hello, {{name}}!").render(c) should equal("Hello, Greg!")
      }
      it("Interprets 'up level' path") {
        sb.compile("Hello, {{player/../name}}!").render(c) should equal("Hello, Greg!")
      }
      it("Interprets dot separated path") {
        sb.compile("Hello, {{player.age}}!").render(c) should equal("Hello, 32!")
      }
      it("Interprets slash separated path") {
        sb.compile("Hello, {{player/age}}!").render(c) should equal("Hello, 32!")
      }
      it("Interprets array path") {
        sb.compile("Hello, {{A.[1].heavy}}!").render(c) should equal("Hello, wicked!")
      }
      it("Interprets 1-element path (unescaped)") {
        sb.compile("Hello, {{{msg}}}!").render(c) should equal("Hello, <p>Yay!</p>!")
      }
    }
    describe("Helper Interpreting") {
      it("Interprets no-arg helper") {
        sb.compile("Hello, {{bar}}!").render(c) should equal("Hello, Simple!")
      }
      it("Interprets simple string-arg helpers") {
        sb.compile("Hello, {{foo \"Greg\"}}!").render(c) should equal("Hello, FooBar Greg!!")
      }
      it("Interprets complex path-arg helpers") {
        sb.compile("Hello, {{foo A.[1].heavy}}!").render(c) should equal("Hello, FooBar wicked!!")
      }
      it("Interprets 2 arg helper") {
        sb.compile("Hello, {{twotags A.[1] 42}}!").render(c) should equal("Hello, Show 42 then wicked!")
      }
      it("Interprets helper with hash string assignments") {
        sb.compile("Hello, {{hash msg=\"Hola\"}}!").render(c) should equal("Hello, Hashed Hola!")
      }
      it("Interprets helper with hash path assignments") {
        sb.compile("Hello, {{hash msg=A.[1].heavy}}!").render(c) should equal("Hello, Hashed wicked!")
      }
      it("Interprets helper with hash path/object assignments") {
        sb.compile("Hello, {{hashObj msg=A.[1]}}!").render(c) should equal("Hello, Hashed wicked!")
      }
      it("Interprets all data types of hash params") {
        sb.compile("Hello, {{allTypes bool=true num= 12.34 nope = null nada=undefined s=\"Greg\"}}!").render(c) should equal("Hello, true 12.34 null undefined Greg!")
      }
      it("Interprets an escaped helper return value") {
        sb.compile("Hello, {{raw}}!").render(c) should equal("Hello, &lt;b&gt;Hey&lt;/b&gt;!")
      }
      it("Interprets an unescaped helper return value") {
        sb.compile("Hello, {{{raw}}}!").render(c) should equal("Hello, <b>Hey</b>!")
      }
      it("Interprets from current context") {
        sb.compile("Hello, {{context}}!").render(c) should equal("Hello, Mike -> Mike!")
      }
    }
    describe("Block Interpreting") {
      it("Simple expr works") {
        sb.compile("Hello, {{#name}}Foo{{/name}}!").render(c) should equal("Hello, Foo!") // exists
        sb.compile("Hello, {{#A.[0].heavy}}Foo{{/A.[0].heavy}}!").render(c) should equal("Hello, Foo!") // exists
        sb.compile("Hello, {{#bogus}}Foo{{/bogus}}!").render(c) should equal("Hello, !") // doesn't exist
        sb.compile("Hello, {{#A.[0].bogus}}Foo{{/A.[0].bogus}}!").render(c) should equal("Hello, !") // doesn't exist
        sb.compile("Hello, {{^bogus}}Foo{{/bogus}}!").render(c) should equal("Hello, Foo!") // doesn't exist
        sb.compile("Hello, {{! comment }}Sarah, nice {{!-- another --}}to see you!").render(c) should equal("Hello, Sarah, nice to see you!")
      }
    }
    describe("Helper Block Interpreting") {
      it("Basic block") {
        sb.compile("Hello, {{#noop}}Foo{{/noop}}!").render(c) should be("Hello, Foo!")
      }
      it("Basic block with context variable") {
        sb.compile("Hello, {{#oneop}}Foo{{/oneop}}!").render(c) should be("Hello, 2_Foo_!")
      }
      it("Basic block with parameter variable") {
        sb.compile("""Hello, {{#strArg "boom"}}Foo{{/strArg}}!""").render(c) should be("Hello, boom _Foo_!")
      }
      it("Iteration block (each example)") {
        sb.compile("""Hello, {{#each A}}Is this {{heavy}}?{{/each}}!""").render(c) should be("Hello, Is this cool?Is this wicked?!")
      }
      it("else block (if example)") {
        sb.compile("""Hello, {{#if A}}here{{else}}missing{{/if}}!""").render(c) should be("Hello, here!")
        sb.compile("""Hello, {{#if bogus}}here{{else}}missing{{/if}}!""").render(c) should be("Hello, missing!")
      }
    }
  }
}
