package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }

class Parsing() extends FunSpec with Matchers {

  val sb = ScalaBars().registerHelper("foo", """function() { return "FooBar "+this.name+"!"; }""")

  describe("------------------------\n:  Handlebars Parsing  :\n------------------------") {
    describe("Thing parsing") {
      it("Parses 1-element path") {
        sb.compile("Hello, {{name}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("name")), true), Text("!")))
      }
      it("Parses 'up level' path") {
        sb.compile("Hello, {{../name}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("..", "name")), true), Text("!")))
      }
      it("Parses dot separated path") {
        sb.compile("Hello, {{emp.author.name}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("emp", "author", "name")), true), Text("!")))
      }
      it("Parses slash separated path") {
        sb.compile("Hello, {{emp/author/name}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("emp", "author", "name")), true), Text("!")))
      }
      it("Parses array path") {
        sb.compile("Hello, {{emp.author.[1].name}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("emp", "author", "[1", "name")), true), Text("!")))
      }
      it("Parses 1-element path (unescaped)") {
        sb.compile("Hello, {{{name}}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("name")), false), Text("!")))
      }
      it("Parses 'up level' path (unescaped)") {
        sb.compile("Hello, {{{../name}}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("..", "name")), false), Text("!")))
      }
      it("Parses dot separated path (unescaped)") {
        sb.compile("Hello, {{{emp.author.name}}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("emp", "author", "name")), false), Text("!")))
      }
      it("Parses slash separated path (unescaped)") {
        sb.compile("Hello, {{{emp/author/name}}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("emp", "author", "name")), false), Text("!")))
      }
      it("Parses array path (unescaped)") {
        sb.compile("Hello, {{{emp.author.[1].name}}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("emp", "author", "[1", "name")), false), Text("!")))
      }
    }
    describe("Helper Parsing") {
      it("Parses no-arg helper") {
        sb.compile("Hello, {{foo}}!").t should equal(List(Text("Hello, "), Thing(SimpleExpr(List("foo")), true), Text("!")))
      }
      it("Parses simple-arg helpers") {
        sb.compile("Hello, {{foo bar blather}}!").t should equal(List(Text("Hello, "), Thing(FullExpr("foo", List(PathArgument(List("bar")), PathArgument(List("blather")))), true), Text("!")))
      }
      it("Parses complex path-arg helpers") {
        sb.compile("Hello, {{foo ../bar blather.[3]/../name}}!").t should equal(List(Text("Hello, "), Thing(FullExpr("foo", List(PathArgument(List("..", "bar")), PathArgument(List("blather", "[3", "..", "name")))), true), Text("!")))
      }
      it("Parses helper with hash string assignments") {
        sb.compile("Hello, {{foo bar name=\"Greg\"}}!").t should equal(List(Text("Hello, "), Thing(FullExpr("foo", List(PathArgument(List("bar")), AssignmentArgument("name", StringArgument("Greg")))), true), Text("!")))
      }
      it("Parses helper with hash path assignments") {
        sb.compile("Hello, {{foo bar name=../author.name}}!").t should equal(List(Text("Hello, "), Thing(FullExpr("foo", List(PathArgument(List("bar")), AssignmentArgument("name", PathArgument(List("..", "author", "name"))))), true), Text("!")))
      }
    }
  }
}
