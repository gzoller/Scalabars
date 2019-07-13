package co.blocke.scalabars

import org.scalatest.{ FunSpec, Matchers }
import model.Context

/**
 * We only test custom Javascript block helpers because non-block helpers are fully tested in SimpleHeader
 * and Scala block headers are sufficiently tested for all the builtin, non-stock helper tests.
 */
class CustomJSHelper() extends FunSpec with Matchers {

  val sb = Scalabars()
  val c = Data(
    "Greg",
    "<p>Yay!</p>",
    15,
    false,
    2L,
    List(Desc("cool"), Desc("wicked")),
    Person("Mike", 32))

  describe("------------------------\n:  Javascript Helpers  :\n------------------------") {
    describe("Handlebars object usage") {
      it("SafeString") {
        val sb2 = sb.registerHelper(
          "safe",
          """
            |function() {
            |  return Handlebars.SafeString("<b>Hey!</b>");
            |}""".stripMargin
        )

        sb2.compile("""Hello, {{safe}}!""")(c) should be("Hello, <b>Hey!</b>!")
      }
      it("escapeExpression") {
        val sb2 = sb.registerHelper(
          "safe",
          """
            |function() {
            |  return Handlebars.escapeExpression("<b>Hey!</b>");
            |}""".stripMargin
        )

        sb2.compile("""Hello, {{{safe}}}!""")(c) should be("Hello, &lt;b&gt;Hey!&lt;/b&gt;!")
      }
    }
    it("Helper using Options object with parameter (array parameter)") {
      val sb2 = sb.registerHelper(
        "jsEach",
        """
          |function(items, options) {
          |  var ret = "";
          |
          |  for(var i=0, j=items.length; i<j; i++) {
          |    ret = ret + options.fn(items);
          |  }
          |
          |  return ret;
          |}""".stripMargin
      )

      sb2.compile("""Hello, {{#jsEach A}}Is this {{this.[1].heavy}}?{{/jsEach}}!""")(c) should be(
        "Hello, Is this wicked?Is this wicked?!")
    }
    it("Helper using Options object with parameter (object parameter) -- 'each' example") {
      val sb2 = sb.registerHelper(
        "jsEach",
        """
          |function(items, options) {
          |  var ret = "";
          |
          |  for(var i=0, j=items.length; i<j; i++) {
          |    ret = ret + options.fn(items[i]);
          |  }
          |
          |  return ret;
          |}""".stripMargin
      )

      sb2.compile("""Hello, {{#jsEach A}}Is this {{heavy}}?{{/jsEach}}!""")(c) should be(
        "Hello, Is this cool?Is this wicked?!")
      // run twice to test caching of compiled code
      sb2.compile("""Hello, {{#jsEach A}}Is this {{heavy}}?{{/jsEach}}!""")(c) should be(
        "Hello, Is this cool?Is this wicked?!")
    }
    it("Same as above but setting a special variable, @index") {
      val sb2 = sb.registerHelper(
        "jsEach",
        """
          |function(arr, options) {
          |  if(!arr || arr.length === 0) {
          |    return options.inverse();
          |  }
          |
          |  var data={};
          |  if( options.data ) {
          |    data = Handlebars.createFrame(options.data);
          |  }
          |
          |  var result = [];
          |  for(var i=0; i<arr.length; i++) {
          |    if(data) {
          |      data.index = i;
          |    }
          |    result.push(options.fn(arr[i], {data: data}));
          |  }
          |
          |  return result.join('');
          |}""".stripMargin
      )
      sb2.compile("""Hello, {{#jsEach A}}Is this {{@index}}. {{heavy}}?{{/jsEach}}!""")(c) should be(
        "Hello, Is this 0. cool?Is this 1. wicked?!")
    }
    it(
      "Same as above (w/special variable) but this time data is preseet (force createFrame() call)") {
        val sb2 = sb
          .registerHelper(
            "outer",
            """
            |function(ctx, options) {
            |  var data={"index":99, "wow":"boom"};
            |  return options.fn(ctx, {data: data});
            |}""".stripMargin
          )
          .registerHelper(
            "inner",
            """
            |function(arr, options) {
            |  var data = {};
            |  if( options.data ){
            |    data = Handlebars.createFrame(options.data)
            |  }
            |  var result = [];
            |  for(var i=0; i<arr.length; i++) {
            |    data.index = i;
            |    result.push(options.fn(arr[i], {data: data}));
            |  }
            |  return result.join('');
            |}
          """.stripMargin
          )
        sb2.compile("""{{#outer this}}
                    |Hello, {{#inner A}}Is {{@wow}} this {{@index}}. {{heavy}}?{{/inner}}!
                    |{{/outer}}""".stripMargin)(c) should be(
          "Hello, Is boom this 0. cool?Is boom this 1. wicked?!\n")
      }
    it("Custom nesting") {
      val json = org.json4s.native.JsonMethods.parse("""
                                                       |{
                                                       |  "interests": [{
                                                       |    "item":"car",
                                                       |    "label":"Porsche 356"
                                                       |  },{
                                                       |    "item":"boat",
                                                       |    "label":"FPB 78"
                                                       |  }]
                                                       |}
        """.stripMargin)
      sb.registerHelper("outerJS", """function(x){ return "Greetings "+x; }""")
        .registerHelper("innerJS", """function(x){ return x.interests[0]; }""") // Scala Object
        .compile("""Hello and {{outerJS (lookup (innerJS this) "label")}}""")(json) should be(
          "Hello and Greetings Porsche 356")
    }
  }
}
