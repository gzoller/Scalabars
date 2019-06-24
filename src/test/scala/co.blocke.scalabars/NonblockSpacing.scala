package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }
import model._

/**
 * There are 16 possible spacing variations indicated by \n before/after the open & close of each block.
 * The combinations are shown below in the 't' declarations.
 */

class NonblockSpacing() extends FunSpec with Matchers {

  val sb = Scalabars()
  val json = org.json4s.native.JsonMethods.parse(
    """
      |{
      |  "title": "My New Post",
      |  "name" : "Greg"
      |}
    """.stripMargin)

  describe("-----------------------\n:  Non-Block Spacing  :\n-----------------------") {
    describe("Simple replacement") {
      it("Normal") {
        sb.compile(
          """This is a
            |{{title}}
            |
            |{{title}}
            | of the system.""".stripMargin)(json) should be("""This is a
                                                                       |My New Post
                                                                       |
                                                                       |My New Post
                                                                       | of the system.""".stripMargin)
      }
      it("No leading ws") {
        sb.compile(
          """This is a
            |{{~title}}
            |
            |{{title}}
            | of the system.""".stripMargin)(json) should be("""This is aMy New Post
                                                               |
                                                               |My New Post
                                                               | of the system.""".stripMargin)
      }
      it("No trailing ws") {
        sb.compile(
          """This is a
            |{{title~}}
            |
            |{{title~}}
            | of the system.""".stripMargin)(json) should be("""This is a
                                                                |My New PostMy New Postof the system.""".stripMargin)
      }
      it("No ws at all") {
        sb.compile(
          """This is a
            |{{~title~}}
            |
            |{{title}}
            | of the system.""".stripMargin)(json) should be("""This is aMy New PostMy New Post
                                                               | of the system.""".stripMargin)
      }
    }
    describe("Block insertion (multi-line inline partial)") {
      describe("Whitespace handling") {
        it("Normal") {
          val t =
            """{{#* inline "nombre"}}
              |A
              |  B -- {{name}}
              |C
              |{{/inline}}
              |My name is:
              |{{>nombre}}
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """My name is:
              |A
              |  B -- Greg
              |C
              |  Say it loud!""".stripMargin)
        }
        it("No leading ws on open tag") {
          val t =
            """x{{#* inline "nombre"}}
              |A
              |  B -- {{name}}
              |C
              |{{/inline}}
              |My name is:
              |{{>nombre}}
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """xMy name is:
              |
              |A
              |  B -- Greg
              |C
              |  Say it loud!""".stripMargin)
        }
        it("No leading ws on close tag") {
          val t =
            """{{#* inline "nombre"}}
              |A
              |  B -- {{name}}
              |C
              |x{{/inline}}
              |My name is:
              |{{>nombre}}
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """
              |My name is:
              |A
              |  B -- Greg
              |C
              |x  Say it loud!""".stripMargin)
        }
        it("No trailing ws on open tag") {
          val t =
            """{{#* inline "nombre"}}x
              |A
              |  B -- {{name}}
              |C
              |{{/inline}}
              |My name is:
              |{{>nombre}}
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """My name is:
              |x
              |A
              |  B -- Greg
              |C
              |  Say it loud!""".stripMargin)
        }
        it("No trailing ws on close tag") {
          val t =
            """{{#* inline "nombre"}}
              |A
              |  B -- {{name}}
              |C
              |{{/inline}}x
              |My name is:
              |{{>nombre}}
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """x
              |My name is:
              |A
              |  B -- Greg
              |C
              |  Say it loud!""".stripMargin)
        }
        it("No ws at all (open/close)") {
          val t =
            """{{#* inline "nombre"}}A
              |  B -- {{name}}
              |C{{/inline}}
              |My name is:
              |{{>nombre}}
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """
              |My name is:
              |A
              |  B -- Greg
              |C  Say it loud!""".stripMargin)
        }
        it("No leading ws on > tag") {
          val t =
            """{{#* inline "nombre"}}
              |A
              |  B -- {{name}}
              |C
              |{{/inline}}
              |My name is:
              |x{{>nombre}}
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """My name is:
              |xA
              |  B -- Greg
              |C
              |
              |  Say it loud!""".stripMargin)
        }
        it("No trailing ws on > tag") {
          val t =
            """{{#* inline "nombre"}}
              |A
              |  B -- {{name}}
              |C
              |{{/inline}}
              |My name is:
              |{{>nombre}}x
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """My name is:
              |A
              |  B -- Greg
              |C
              |x
              |  Say it loud!""".stripMargin)
        }
        it("No ws at all on > tag") {
          val t =
            """{{#* inline "nombre"}}
              |A
              |  B -- {{name}}
              |C
              |{{/inline}}
              |My name is:
              |x{{>nombre}}y
              |  Say it loud!""".stripMargin
          sb.compile(t)(json) should be(
            """My name is:
              |xA
              |  B -- Greg
              |C
              |y
              |  Say it loud!""".stripMargin)
        }
      }
    }
  }
}
