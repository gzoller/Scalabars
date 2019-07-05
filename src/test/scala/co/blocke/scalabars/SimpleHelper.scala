package co.blocke.scalabars

import co.blocke.scalabars.model._
import org.json4s._
import org.scalatest.{ FunSpec, Matchers }

class SimpleHelper() extends FunSpec with Matchers {

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
                                                   |  "numbers":[5,6,7,8],
                                                   |  "numberSet":[[5,7],[8,9]],
                                                   |  "player":{
                                                   |    "name": "David",
                                                   |    "age": 12
                                                   |  }
                                                   |}
    """.stripMargin)

  val sb = Scalabars()

  describe("--------------------------\n:  Simple Custom Helper  :\n--------------------------") {
    describe("Scala") {
      it("Simple no-arg helper") {
        sb.registerHelper("noargScala", H1())
          .compile("Hello and {{noargScala}}")(json) should be("Hello and Greetings!")
      }
      it("Simple helper with literal arguments") {
        sb.registerHelper("litargScala", H2())
          .compile("""Hello and {{litargScala "Foo" 35}}""")(json) should be(
            "Hello and Greetings Foo at 35!")
      }
      it("Simple helper with path (evals to scalar) argument") {
        sb.registerHelper("patharg1Scala", H3())
          .compile("""Hello and {{patharg1Scala interests.0.item}}""")(json) should be(
            "Hello and Greetings car!")
      }
      it("Simple helper with path (evals to context) argument") {
        sb.registerHelper("patharg2Scala", H4())
          .compile("""Hello and {{patharg2Scala interests}}""")(json) should be(
            "Hello and Greetings FPB 78!")
      }
      it("Simple helper with assignment argument") {
        sb.registerHelper("assignJS", H5())
          .compile("""Hello and {{assignJS tidal="wave"}}""")(json) should be(
            "Hello and Greetings wave!")
      }
      it("Simple (nested) helper with expression (evals to scalar) argument") {
        sb.registerHelper("outerScala", H6a())
          .registerHelper("innerScala", H6b())
          .compile("""Hello and {{outerScala (innerScala name)}}""")(json) should be(
            "Hello and Greetings Greg!")
      }
      describe("Simple helper with expression (evals to context) argument (nested helpers)") {
        it("Inner expression resolves to List literal") {
          sb.registerHelper("outerScala", H7a())
            .registerHelper("innerScala", H7b())
            .compile("""Hello and {{outerScala (innerScala this)}}""")(json) should be(
              "Hello and Greetings b")
        }
        it("Inner expression resolves to Object literal") {
          sb.registerHelper("outerScala", H8a())
            .registerHelper("innerScala", H8b())
            .compile("""Hello and {{outerScala (innerScala)}}""")(json) should be(
              "Hello and Greetings bar")
        }
        it("Inner expression resolves to List[List]") {
          sb.registerHelper("outerJS", H9a())
            .registerHelper("innerJS", H9b())
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings c")
        }
        it("Inner expression resolves to List[Object]") {
          sb.registerHelper("outerJS", H10a())
            .registerHelper("innerJS", H10b())
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala List") {
          sb.registerHelper("outerJS", H11a())
            .registerHelper("innerJS", H11b())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala Object") {
          sb.registerHelper("outerJS", H12a())
            .registerHelper("innerJS", H12b())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings David")
        }
        it("Inner expression resolves to Scala List[List]") {
          sb.registerHelper("outerJS", H13a())
            .registerHelper("innerJS", H13b())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 8")
        }
        it("Inner expression resolves to Scala List[Object]") {
          sb.registerHelper("outerJS", H14a())
            .registerHelper("innerJS", H14b())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings FPB 78")
        }
      }
    }
    describe("Javascript") {
      it("Simple no-arg helper") {
        sb.registerHelper("noargJS", """function(){ return "Greetings!"; }""")
          .compile("Hello and {{noargJS}}")(json) should be("Hello and Greetings!")
      }
      it("Simple helper with literal arguments") {
        sb.registerHelper("litargJS", """function(x, y){ return "Greetings "+x+" at "+y+"!"; }""")
          .compile("""Hello and {{litargJS "Foo" 35}}""")(json) should be(
            "Hello and Greetings Foo at 35!")
      }
      it("Simple helper with path (evals to scalar) argument") {
        sb.registerHelper("patharg1JS", """function(x){ return "Greetings "+x+"!"; }""")
          .compile("""Hello and {{patharg1JS interests.0.item}}""")(json) should be(
            "Hello and Greetings car!")
      }
      it("Simple helper with path (evals to context) argument") {
        sb.registerHelper("patharg2JS", """function(x){ return "Greetings "+x[1].label+"!"; }""")
          .compile("""Hello and {{patharg2JS interests}}""")(json) should be(
            "Hello and Greetings FPB 78!")
      }
      it("Simple helper with assignment argument") {
        sb.registerHelper(
          "assignJS",
          """function(options){ return "Greetings "+ options.hash["tidal"]+"!"; }""")
          .compile("""Hello and {{assignJS tidal="wave"}}""")(json) should be(
            "Hello and Greetings wave!")
      }
      it("Simple (nested) helper with expression (evals to scalar) argument") {
        sb.registerHelper("outerJS", """function(x){ return "Greetings "+x; }""")
          .registerHelper("innerJS", """function(x){ return x+"!"; }""")
          .compile("""Hello and {{outerJS (innerJS name)}}""")(json) should be(
            "Hello and Greetings Greg!")
      }
      describe("Simple helper with expression (evals to context) argument (nested helpers)") {
        it("Inner expression resolves to Javascript List literal") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1]; }""")
            .registerHelper("innerJS", """function(){ return ["a","b","c"]; }""")
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings b")
        }
        it("Inner expression resolves to Javascript Object literal") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x.foo; }""")
            .registerHelper("innerJS", """function(){ return {"foo":"bar"}; }""")
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be(
              "Hello and Greetings bar")
        }
        it("Inner expression resolves to Javascript List[List]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1][0]; }""")
            .registerHelper("innerJS", """function(){ return [["a","b"],["c","d"]]; }""")
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings c")
        }
        it("Inner expression resolves to Javascript List[Object]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1].b; }""")
            .registerHelper("innerJS", """function(){ return [{"a":3,"b":4},{"a":5,"b":6}]; }""")
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala List") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1]; }""")
            .registerHelper("innerJS", """function(x){ return x.numbers; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala Object") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x.name; }""")
            .registerHelper("innerJS", """function(x){ return x.player; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings David")
        }
        it("Inner expression resolves to Scala List[List]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1][0]; }""")
            .registerHelper("innerJS", """function(x){ return x.numberSet; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 8")
        }
        it("Inner expression resolves to Scala List[Object]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1].label; }""")
            .registerHelper("innerJS", """function(x){ return x.interests; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings FPB 78")
        }
      }
    }
    describe(
      "Hybrid Scala Outer/JS Inner Simple helper with expression (evals to context) argument (nested helpers)") {
        it("Inner expression resolves to List literal") {
          sb.registerHelper("outerScala", H15())
            .registerHelper("innerJS", """function(){ return ["a","b","c"]; }""")
            .compile("""Hello and {{outerScala (innerJS)}}""")(json) should be(
              "Hello and Greetings b")
        }
        it("Inner expression resolves to Object literal") {
          sb.registerHelper("outerJS", H16())
            .registerHelper("innerJS", """function(){ return {"foo":"bar"}; }""")
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings bar")
        }
        it("Inner expression resolves to Javascript List[List]") {
          sb.registerHelper("outerJS", H17())
            .registerHelper("innerJS", """function(){ return [["a","b"],["c","d"]]; }""")
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings c")
        }
        it("Inner expression resolves to Javascript List[Object]") {
          sb.registerHelper("outerJS", H18())
            .registerHelper("innerJS", """function(){ return [{"a":3,"b":4},{"a":5,"b":6}]; }""")
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala List") {
          sb.registerHelper("outerJS", H19())
            .registerHelper("innerJS", """function(x){ return x.numbers; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala Object") {
          sb.registerHelper("outerJS", H20())
            .registerHelper("innerJS", """function(x){ return x.player; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings David")
        }
        it("Inner expression resolves to Scala List[List]") {
          sb.registerHelper("outerJS", H21())
            .registerHelper("innerJS", """function(x){ return x.numberSet; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 8")
        }
        it("Inner expression resolves to Scala List[Object]") {
          sb.registerHelper("outerJS", H22())
            .registerHelper("innerJS", """function(x){ return x.interests; }""")
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings FPB 78")
        }
      }
    describe(
      "Hybrid JS Outer/Scala Inner Simple helper with expression (evals to context) argument (nested helpers)") {
        it("Inner expression resolves to Javascript List") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1]; }""")
            .registerHelper("innerJS", H23())
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings b")
        }
        it("Inner expression resolves to Javascript Object") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x.foo; }""")
            .registerHelper("innerJS", H24())
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings bar")
        }
        it("Inner expression resolves to Javascript List[List]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1][0]; }""")
            .registerHelper("innerJS", H25())
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings c")
        }
        it("Inner expression resolves to Javascript List[Object]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1].b; }""")
            .registerHelper("innerJS", H26())
            .compile("""Hello and {{outerJS (innerJS)}}""")(json) should be("Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala List") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1]; }""")
            .registerHelper("innerJS", H27())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 6")
        }
        it("Inner expression resolves to Scala Object") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x.name; }""")
            .registerHelper("innerJS", H28())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings David")
        }
        it("Inner expression resolves to Scala List[List]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1][0]; }""")
            .registerHelper("innerJS", H29())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings 8")
        }
        it("Inner expression resolves to Scala List[Object]") {
          sb.registerHelper("outerJS", """function(x){ return "Greetings "+x[1].label; }""")
            .registerHelper("innerJS", H30())
            .compile("""Hello and {{outerJS (innerJS this)}}""")(json) should be(
              "Hello and Greetings FPB 78")
        }
      }
  }
}

case class H1() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    "Greetings!"
}
case class H2() extends Helper("x", "y") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x")} at ${arg("y")}!"""
}
case class H3() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x")}!"""
}
case class H4() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"Greetings ${(arg("x") >> StringEvalResult("1.label")).get}!"
}
case class H5() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${options.hash("tidal")}!"""
}
case class H6a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x")}"""
}
case class H6b() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""${arg("x")}!"""
}
case class H7a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> LongEvalResult(1)).get}"""
}
case class H7b() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    List("a", "b", "c")
}
case class H8a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> StringEvalResult("foo")).get}"""
}
case class H8b() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    Map("foo" -> "bar")
}
case class H9a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> StringEvalResult("[1].[0]")).get}"""
}
case class H9b() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    List(List("a", "b"), List("c", "d"))
}
case class H10a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> StringEvalResult("1.b")).get}"""
}
case class H10b() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    List(Map("a" -> 3, "b" -> 4), Map("a" -> 5, "b" -> 6))
}
case class H11a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> StringEvalResult("[1]")).get}"""
}
case class H11b() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("numbers")).get
}
case class H12a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> StringEvalResult("name")).get}"""
}
case class H12b() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("player")).get
}
case class H13a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> StringEvalResult("1.0")).get}"""
}
case class H13b() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("numberSet")).get
}
case class H14a() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val z = (arg("x") >> StringEvalResult("1.label")).get
    s"""Greetings $z"""
  }
}
case class H14b() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("interests")).get
}
case class H15() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${(arg("x") >> StringEvalResult("[1]")).get}"""
}
case class H16() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x.foo")}"""
}
case class H17() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x.[1].[0]")}"""
}
case class H18() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x.[1].b")}"""
}
case class H19() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x.[1]")}"""
}
case class H20() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x.name")}"""
}
case class H21() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x.[1].0")}"""
}
case class H22() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    s"""Greetings ${arg("x.[1].label")}"""
}
case class H23() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    List("a", "b", "c")
}
case class H24() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    Map("foo" -> "bar")
}
case class H25() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    List(List("a", "b"), List("c", "d"))
}
case class H26() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    List(Map("a" -> 3, "b" -> 4), Map("a" -> 5, "b" -> 6))
}
case class H27() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("numbers")).get
}
case class H28() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("player")).get
}
case class H29() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("numberSet")).get
}
case class H30() extends Helper("x") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    (arg("x") >> StringEvalResult("interests")).get
}
