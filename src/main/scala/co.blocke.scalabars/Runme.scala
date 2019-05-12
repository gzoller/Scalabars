package co.blocke.scalabars

import org.graalvm.polyglot.{ Context => GraalContext }

case class Person(name: String, age: Int)
case class Desc(heavy: String)
case class Data(
    name:   String,
    msg:    String,
    aNum:   Int,
    isOK:   Boolean,
    small:  Long,
    A:      List[Desc],
    player: Person
)

import javax.script._

case class Foo() {
  def hi(name: String): String = "Hi, " + name + "!"
}

object Runme extends App {

  val sb = Scalabars()

  val t = """{{#sortEach collection}}Foo: {{this}} {{/sortEach}}"""

  val data = Map("collection" -> List("Harry", "Sally", "Larry"))
  val data2 = Map("object" -> Map("one" -> 1, "two" -> 2))

  println(sb.compile(t).render(data))

  /*
  println("Version: " + System.getProperty("java.vendor") + " " + System.getProperty("java.version"))
  val engine = new ScriptEngineManager().getEngineByName("nashorn")
  //  val engine = new ScriptEngineManager().getEngineByName("graal.js")

  val bindings = engine.createBindings()
  bindings.put("foo", Foo())
  engine.setBindings(bindings, ScriptContext.ENGINE_SCOPE)

  engine.eval("var myFn = function(name){return this.x + ' :: ' + foo.hi(name);}")
  println("Say " + engine.eval("""myFn.call({"x":"bar"},'Fred')""")) // "Say Hi, Fred!"

  println("Is x? " + engine.eval("this"))
  */
}

//"""{"foo":"bar"}"""