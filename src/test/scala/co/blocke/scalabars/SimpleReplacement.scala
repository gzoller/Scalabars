package co.blocke.scalabars

import co.blocke.scalabars.model._
import org.json4s._
import org.scalatest.{ FunSpec, Matchers }

class SimpleReplacement() extends FunSpec with Matchers {

  val json = org.json4s.native.JsonMethods.parse("""
                                                   |{
                                                   |  "name": "Greg",
                                                   |  "age": 53,
                                                   |  "ok": true,
                                                   |  "interests": [{
                                                   |    "item":"car",
                                                   |    "label":"Porsche 356"
                                                   |  },{
                                                   |    "item":"boat",
                                                   |    "label":"FPB 78"
                                                   |  }],
                                                   |  "numbers":[5,7,9],
                                                   |  "player":{
                                                   |    "name": "David",
                                                   |    "age": 12
                                                   |  }
                                                   |}
    """.stripMargin)

  val sb = Scalabars()

  describe("--------------------------------\n:  Simple Element Replacement  :\n--------------------------------") {
    it("Simple scalar element") {
      sb.compile("Hey {{name}}!")(json) should be("Hey Greg!")
    }
    it("Multi-layer down") {
      sb.compile("Hey {{player.age}}!")(json) should be("Hey 12!")
    }
    it("Array indexing") {
      sb.compile("Hey {{interests.[1]/../[0].label}}!")(json) should be("Hey Porsche 356!")
    }
    it("With backtracking") {
      sb.compile("Hey {{interests/../ok}}!")(json) should be("Hey true!")
    }
    it("Missing field (non-strict)") {
      sb.compile("Hey {{bogus}}!")(json) should be("Hey !")
    }
    it("Missing field (strict)") {
      the[BarsException] thrownBy sb.compile("Hey {{bogus}}!", Map("strict" -> true))(json) should have message "Path or helper not found: bogus"
    }
  }
}
