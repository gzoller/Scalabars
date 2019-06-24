package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }
import model._

/**
 * There are 16 possible spacing variations indicated by \n before/after the open & close of each block.
 * The combinations are shown below in the 't' declarations.
 *
 * Positions for WS ctl:
 * {{(1)# foo(2)}}
 * A
 * B
 * C
 * {{(3)/foo(4)}}
 */

class BlockSpacing() extends FunSpec with Matchers {

  val sb = Scalabars()
  val json = org.json4s.native.JsonMethods.parse(
    """
      |{
      |  "title": "My New Post",
      |  "name" : "Greg"
      |}
    """.stripMargin)

  describe("-------------------\n:  Block Spacing  :\n-------------------") {
    describe("Simple replacement") {
      it("Normal (open tag first)") {
        sb.compile(
          """{{# name}}
            |What'cha {{this}} doin?
            |{{/name}}
            |Done""".stripMargin)(json) should be("""What'cha Greg doin?
                                                     |Done""".stripMargin)
      }
      it("Leading ws (no NL)") {
        sb.compile(
          """   {{# name}}
            |What'cha {{this}} doin?
            |{{/name}}
            |Done""".stripMargin)(json) should be("""What'cha Greg doin?
                                                    |Done""".stripMargin)
      }
      it("Leading ws (with NL)") {
        // @formatter:off
        sb.compile(
          """
            |  {{# name}}
            |What'cha {{this}} doin?
            |{{/name}}
            |Done""".stripMargin)(json) should be("""
                                                    |What'cha Greg doin?
                                                    |Done""".stripMargin)
        // @formatter:on
      }
      it("Open tag not clear/alone on line") {
        sb.compile(
          """abc{{# name}}
            |What'cha {{this}} doin?
            |{{/name}}
            |Done""".stripMargin)(json) should be("""abc
                                                    |What'cha Greg doin?
                                                    |Done""".stripMargin)
      }
      it("Trailing char on open tag line") {
        sb.compile(
          """{{# name}}x
            |What'cha {{this}} doin?
            |{{/name}}
            |Done""".stripMargin)(json) should be("""x
                                                    |What'cha Greg doin?
                                                    |Done""".stripMargin)
      }
      it("Close tag has preceding char") {
        sb.compile(
          """{{# name}}
            |What'cha {{this}} doin?
            |x{{/name}}
            |Done""".stripMargin)(json) should be("""What'cha Greg doin?
                                                    |x
                                                    |Done""".stripMargin)
      }
      it("Close tag has trailing char") {
        sb.compile(
          """{{# name}}
            |What'cha {{this}} doin?
            |{{/name}}x
            |Done""".stripMargin)(json) should be("""What'cha Greg doin?
                                                    |x
                                                    |Done""".stripMargin)
      }
    }
    describe("Whitespace control") {
      it("Before open tag") {
        sb.compile(
          """
            |  {{~# name}}
            |What'cha {{this}} doin?
            |{{/name}}
            |Done""".stripMargin)(json) should be("""What'cha Greg doin?
                                                    |Done""".stripMargin)
      }
      it("After open tag") {
        sb.compile(
          """x{{# name~}}
            |What'cha {{this}} doin?
            |{{/name}}
            |Done""".stripMargin)(json) should be("""xWhat'cha Greg doin?
                                                    |Done""".stripMargin)
      }
      it("Before close tag") {
        sb.compile(
          """{{# name}}
            |What'cha {{this}} doin?
            |{{~/name}}
            |Done""".stripMargin)(json) should be("""What'cha Greg doin?Done""")
      }
      it("After close tag") {
        sb.compile(
          """{{# name}}
            |What'cha {{this}} doin?
            |{{/name~}}
            |
            |  Done""".stripMargin)(json) should be("""What'cha Greg doin?
                                                    |Done""".stripMargin)
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