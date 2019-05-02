package co.blocke.scalabars

import reflect.runtime.universe.TypeTag
import javax.script.ScriptEngineManager
import org.json4s.native.JsonMethods

case class Template(t: List[Renderable])(implicit sb: ScalaBars) {
  def render[T](context: T)(implicit tt: TypeTag[T]): String = render(SB.sj.render(context))
  def render(context: SB.Scope): String = t.map(_.render(Context(context))).mkString("")
}

case class ScalaBars(
    partials: Map[String, String] = Map.empty[String, String],
    helpers:  Map[String, String] = Map.empty[String, String]) {

  private val parser = Bars2() // HandlebarsParser()
  private val javascript = new ScriptEngineManager().getEngineByName("nashorn")
  private var compiledPartials = Map.empty[String, List[Renderable]]
  private var helpersEvaled = false

  def registerPartial(name: String, template: String): ScalaBars = this.copy(partials = this.partials + (name -> template))
  def registerHelper(name: String, code: String): ScalaBars = this.copy(helpers = this.helpers + (name -> code))

  private def compilePartialsAndHelpers() = {
    if (compiledPartials.isEmpty && partials.nonEmpty)
      compiledPartials = partials.map { case (k, v) => (k, parser.compile(v)) }
    if (!helpersEvaled && helpers.nonEmpty) {
      helpers.map { case (k, v) => javascript.eval(s"var $k = " + v) }
      helpersEvaled = true
    }
  }

  def compile(t: String) = {
    compilePartialsAndHelpers()
    Template(parser.compile(t))(this)
  }

  private[scalabars] def getPartial(name: String): Option[List[Renderable]] = compiledPartials.get(name)

  private[scalabars] def run(
      fnName:  String,
      context: Context,
      body:    Option[List[Renderable]],
      args:    String*): String = {
    val contextVars = JsonMethods.compact(JsonMethods.render(context.value))
    val argsStr = if (args.size == 0) "" else s", ${args.mkString(",")}"
    javascript.eval(
      s"""$fnName.call($contextVars$argsStr)"""
    ).toString
  }

  private[scalabars] def run2(
      expr:    FullExpr,
      context: Context,
      body:    Option[List[Renderable]]): String = {

    val (assignments, otherArgs) = expr.args.partition(_.isInstanceOf[AssignmentArgument])
    val hashStr = stringifyAssignments(assignments.asInstanceOf[List[AssignmentArgument]], context)
    println("Expr label: " + expr.label)
    val params = otherArgs.map(_ match {
      case s: StringArgument => "\"" + s.value + "\""
      case p: PathArgument   => JsonMethods.compact(JsonMethods.render(context.find(p.path).value))
    }).mkString(",", ",", "")
    val argStr = if (params == ",") "" else params
    println("Hash Str: " + hashStr + argStr)
    println("    Script--> " + s"""${expr.label}.call($hashStr$argStr)""")
    javascript.eval(
      s"""${expr.label}.call($hashStr$argStr)"""
    ).toString
  }

  private[scalabars] def parseMe(raw: String): List[Renderable] = parser.compile(raw)

  private def stringifyAssignments(args: List[AssignmentArgument], context: Context) =
    args.map(a => a.value match {
      case s: StringArgument => s""""${a.label}":${stringifyValue(s.value)}"""
      case p: PathArgument   => s""""${a.label}":""" + JsonMethods.compact(JsonMethods.render(context.find(p.path).value))
    }).mkString("{", ",", "}")

  //numbers, strings, true, false, null and undefined
  import SB._
  private def stringifyValue(v: String) = v match {
    case "true" | "false" | "null" | "undefined" => v
    case _ if v.isNumeric()                      => v
    case _                                       => "\"" + v + "\""
  }
}

/*

var a = {"name":"Greg","OK":true};
var b = {"foo":[1,2,3]};
var c = Object.assign({}, a, b);


import javax.script.*;

public class ExecuteScript {

    public static void main(String[] args) throws Exception {
        // create a Java object
        ExecuteScript es = new ExecuteScript();

        // create a script engine manager
        ScriptEngineManager factory = new ScriptEngineManager();
        // create a JavaScript engine
        ScriptEngine engine = factory.getEngineByName("JavaScript");
        // evaluate JavaScript code from String
        engine.eval("println('Welcome to Java world')");

        // add the Java object into the engine.
        engine.put("es",es);

        ScriptEngineFactory sef = engine.getFactory();
        String s = sef.getMethodCallSyntax("es", "sayHi", new String[0]);
        // show the correct way to call the Java method
        System.out.println(s);
        engine.eval(s);
    }

    public static void sayHi(){
        System.out.println("hihi");
    }
}

 */ 