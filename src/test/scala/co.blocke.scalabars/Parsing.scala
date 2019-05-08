package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Parsing() extends FunSpec with Matchers {

  val sb = Scalabars()

  describe("------------------------\n:  Handlebars Parsing  :\n------------------------") {
    describe("Thing parsing") {
      it("Parses 1-element path") {
        sb.compile("Hello, {{name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("name"), List(), true), Text("!")))
      }
      it("Parses 'up level' path") {
        sb.compile("Hello, {{../name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("..", "name"), List(), true), Text("!")))
      }
      it("Parses dot separated path") {
        sb.compile("Hello, {{emp.author.name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("emp", "author", "name"), List(), true), Text("!")))
      }
      it("Parses slash separated path") {
        sb.compile("Hello, {{emp/author/name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("emp", "author", "name"), List(), true), Text("!")))
      }
      it("Parses array path") {
        sb.compile("Hello, {{emp.author.[1].name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("emp", "author", "[1", "name"), List(), true), Text("!")))
      }
      it("Parses 1-element path (unescaped)") {
        sb.compile("Hello, {{{name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("name"), List(), false), Text("!")))
      }
      it("Parses 'up level' path (unescaped)") {
        sb.compile("Hello, {{{../name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("..", "name"), List(), false), Text("!")))
      }
      it("Parses dot separated path (unescaped)") {
        sb.compile("Hello, {{{emp.author.name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("emp", "author", "name"), List(), false), Text("!")))
      }
      it("Parses slash separated path (unescaped)") {
        sb.compile("Hello, {{{emp/author/name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("emp", "author", "name"), List(), false), Text("!")))
      }
      it("Parses array path (unescaped)") {
        sb.compile("Hello, {{{emp.author.[1].name}}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("emp", "author", "[1", "name"), List(), false), Text("!")))
      }
      it("Parses current level") {
        sb.compile("Hello, {{./name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List(".", "name"), List(), true), Text("!")))
      }
    }
    describe("Helper Parsing") {
      it("Parses no-arg helper") {
        sb.compile("Hello, {{each}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("each"), List(), true), Text("!")))
      }
      it("Parses simple-arg helpers") {
        sb.compile("Hello, {{foo bar blather}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("foo"), List(PathArgument(List("bar")), PathArgument(List("blather"))), true), Text("!")))
      }
      it("Parses complex path-arg helpers") {
        sb.compile("Hello, {{foo ../bar blather.[3]/../name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("foo"), List(PathArgument(List("..", "bar")), PathArgument(List("blather", "[3", "..", "name"))), true), Text("!")))
      }
      it("Parses helper with hash string assignments") {
        sb.compile("Hello, {{foo bar name=\"Greg\"}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("foo"), List(PathArgument(List("bar")), AssignmentArgument("name", StringArgument("Greg"))), true), Text("!")))
      }
      it("Parses helper with hash path assignments") {
        sb.compile("Hello, {{foo bar name=../author.name}}!").compiled should equal(List(Text("Hello, "), SimpleExpression(List("foo"), List(PathArgument(List("bar")), AssignmentArgument("name", PathArgument(List("..", "author", "name")))), true), Text("!")))
      }
    }
    describe("Block Parsing") {
      it("Parses 1-element path") {
        sb.compile("Hello, {{#name}}Foo{{/name}}!").compiled should equal(List(Text("Hello, "), BlockExpression(List("name"), List(), List(Text("Foo")), false), Text("!")))
      }
      it("Inverted") {
        sb.compile("Hello, {{^name}}Foo{{/name}}!").compiled should equal(List(Text("Hello, "), BlockExpression(List("name"), List(), List(Text("Foo")), true), Text("!")))
      }
    }
  }
}
