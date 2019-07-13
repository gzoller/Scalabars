package co.blocke.scalabars

import org.json4s._
import org.scalatest.{ FunSpec, Matchers }
import model._

class PathNavigation() extends FunSpec with Matchers {

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
  val o = Options(sb)
  val root = Context.root(json)

  describe("-------------------------------\n:  Path Navigation (Context)  :\n-------------------------------") {
    describe("Lookup") {
      it("Root path") {
        root.show should be("""/ --> JObject(List((name,JString(Greg)), (age,JInt(53)), (ok,JBool(true)), (inter...
                              |   Stack:
                              |   Data: Map()
                              |   Partials: """.stripMargin)
      }
      it("Simple element lookup") {
        root.lookup("/interests").show should be("""/interests --> JArray(List(JObject(List((item,JString(car)), (label,JString(Porsche 356)))...
                                                   |   Stack:
                                                   |      / --> JObject(List((name,JString(Greg)), (age,JInt(53)), (ok,JBool(true)), (inter...
                                                   |   Data: Map()
                                                   |   Partials: """.stripMargin)
      }
      it("Backgrack lookup (from non-root node)") {
        root.lookup("/interests").lookup("../ok").show should be(
          """/ok --> JBool(true)...
            |   Stack:
            |      /interests --> JArray(List(JObject(List((item,JString(car)), (label,JString(Porsche 356)))...
            |      / --> JObject(List((name,JString(Greg)), (age,JInt(53)), (ok,JBool(true)), (inter...
            |   Data: Map()
            |   Partials: """.stripMargin)
      }
      it("Backgrack lookup (from root node)") {
        root.lookup("/interests/../ok").show should be("""/ok --> JBool(true)...
                                                         |   Stack:
                                                         |      / --> JObject(List((name,JString(Greg)), (age,JInt(53)), (ok,JBool(true)), (inter...
                                                         |   Data: Map()
                                                         |   Partials: """.stripMargin)
      }
      it("Path item not found") {
        root.lookup("bogus").show should be("""/bogus --> JNothing...
                                              |   Stack:
                                              |      / --> JObject(List((name,JString(Greg)), (age,JInt(53)), (ok,JBool(true)), (inter...
                                              |   Data: Map()
                                              |   Partials: """.stripMargin)
      }
    }
    describe("Render") {
      it("Root (Context render)") {
        sb.compile("{{this}}")(json) should be("[object Object]")
      }
      it("Simple element lookup") {
        sb.compile("{{/interests}}")(json) should be("[object Object],[object Object]")
      }
      it("Backgrack lookup") {
        sb.compile("{{this}}").render(root.lookup("/interests").lookup("../ok")) should be("true")
      }
      it("Path item not found (non-strict)") {
        sb.compile("{{bogus}}")(json) should be("")
      }
      it("Path item not found (strict)") {
        the[BarsException] thrownBy sb.compile("{{bogus}}", Map("strict" -> true))(json) should have message "Path or helper not found: bogus"
      }
    }
  }
}
