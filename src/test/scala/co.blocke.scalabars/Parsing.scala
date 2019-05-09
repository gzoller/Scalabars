package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Parsing() extends FunSpec with Matchers {

  val sb = Scalabars()

  describe("------------------------\n:  Handlebars Parsing  :\n------------------------") {
    describe("Thing parsing") {
      it("Parses 1-element path") {
        sb.compile("Hello, {{name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("name", List("name"), List(), true), Text("!")))
      }
      it("Parses 'up level' path") {
        sb.compile("Hello, {{../name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("../name", List("..", "name"), List(), true), Text("!")))
      }
      it("Parses dot separated path") {
        sb.compile("Hello, {{emp.author.name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("emp.author.name", List("emp", "author", "name"), List(), true), Text("!")))
      }
      it("Parses slash separated path") {
        sb.compile("Hello, {{emp/author/name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("emp/author/name", List("emp", "author", "name"), List(), true), Text("!")))
      }
      it("Parses array path") {
        sb.compile("Hello, {{emp.author.[1].name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("emp.author.[1].name", List("emp", "author", "[1]", "name"), List(), true), Text("!")))
      }
      it("Parses 1-element path (unescaped)") {
        sb.compile("Hello, {{{name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("name", List("name"), List(), false), Text("!")))
      }
      it("Parses 'up level' path (unescaped)") {
        sb.compile("Hello, {{{../name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("../name", List("..", "name"), List(), false), Text("!")))
      }
      it("Parses dot separated path (unescaped)") {
        sb.compile("Hello, {{{emp.author.name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("emp.author.name", List("emp", "author", "name"), List(), false), Text("!")))
      }
      it("Parses slash separated path (unescaped)") {
        sb.compile("Hello, {{{emp/author/name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("emp/author/name", List("emp", "author", "name"), List(), false), Text("!")))
      }
      it("Parses array path (unescaped)") {
        sb.compile("Hello, {{{emp.author.[1].name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("emp.author.[1].name", List("emp", "author", "[1]", "name"), List(), false), Text("!")))
      }
      it("Parses current level") {
        sb.compile("Hello, {{./name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("./name", List(".", "name"), List(), true), Text("!")))
      }
    }
    describe("Helper Parsing") {
      it("Parses no-arg helper") {
        sb.compile("Hello, {{each}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("each", List("each"), List(), true), Text("!")))
      }
      it("Parses simple-arg helpers") {
        sb.compile("Hello, {{foo bar blather}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("foo", List("foo"), List(PathArgument(List("bar")), PathArgument(List("blather"))), true), Text("!")))
      }
      it("Parses complex path-arg helpers") {
        sb.compile("Hello, {{foo ../bar blather.[3]/../name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("foo", List("foo"), List(PathArgument(List("..", "bar")), PathArgument(List("blather", "[3]", "..", "name"))), true), Text("!")))
      }
      it("Parses helper with hash string assignments") {
        sb.compile("Hello, {{foo bar name=\"Greg\"}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("foo", List("foo"), List(PathArgument(List("bar")), AssignmentArgument("name", StringArgument("Greg"))), true), Text("!")))
      }
      it("Parses helper with hash path assignments") {
        sb.compile("Hello, {{foo bar name=../author.name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression("foo", List("foo"), List(PathArgument(List("bar")), AssignmentArgument("name", PathArgument(List("..", "author", "name")))), true), Text("!")))
      }
    }
    describe("Block Parsing") {
      it("Parses 1-element path") {
        sb.compile("Hello, {{#name}}Foo{{/name}}!").compiled should equal(List(Text("Hello, "), BlockExpression("name", List("name"), List(), List(Text("Foo")), false), Text("!")))
      }
      it("Inverted") {
        sb.compile("Hello, {{^name}}Foo{{/name}}!").compiled should equal(List(Text("Hello, "), BlockExpression("name", List("name"), List(), List(Text("Foo")), true), Text("!")))
      }
    }
    describe("Parse failures") {
      it("Path parsing fails") {
        the[BarsException] thrownBy sb.parsePath("$%ads.*(") should have message "Path parsing failed: Parsed.Failure(Position 1:1, found \"$%ads.*(\")"
      }
      it("Template parsing fails") {
        the[BarsException] thrownBy sb.compile("Wow {{this} is great!") should have message "Template parsing failed: Parsed.Failure(Position 1:11, found \"} is great\")"
      }
    }
  }
}
