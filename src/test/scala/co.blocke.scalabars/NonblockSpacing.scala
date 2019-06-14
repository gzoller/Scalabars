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
        sb.compile(t)(json) should be("""My name is:
                                        |A
                                        |  B -- Greg
                                        |C
                                        |  Say it loud!""".stripMargin)
      }
      it("No leading ws") {
        val t =
          """x{{#* inline "nombre"}}
            |A
            |  B -- {{name}}
            |C
            |{{/inline}}
            |My name is:
            |{{>nombre}}
            |  Say it loud!""".stripMargin
        println(sb.compile(t)(json))
        sb.compile(t)(json) should be("""xMy name is:
                                        |
                                        |A
                                        |  B -- Greg
                                        |C
                                        |  Say it loud!""".stripMargin)
      }
      it("No trailing ws") {
        (pending)
      }
      it("No ws at all") {
        (pending)
      }
    }
  }
}

/*
My name is:

A
  B --
C
Say it loud!
 */ 