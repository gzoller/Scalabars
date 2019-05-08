package co.blocke.scalabars

case class FooHelper() extends Helper(List("tag")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "FooBar " + resolve("tag") + "!"
}

case class TwoTagsHelper() extends Helper(List("a", "b")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "Show " + resolve("b") + " then " + resolve("a.heavy")
}

case class BarHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "Simple"
}

case class HashHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "Hashed " + resolve("this.msg")
}

case class HashObjHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "Hashed " + resolve("this.msg.heavy")
}

case class AllTypesHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    resolve("this.bool") +
      " " + resolve("this.num") +
      " " + resolve("this.nope") +
      " " + resolve("this.nada") +
      " " + resolve("this.s")
}

case class RawHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "<b>Hey</b>"
}

case class ContextHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    Handlebars.SafeString(resolve("this.player.name") + " -> " + resolve("player.name"))
}

/*
    .registerHelper("foo", """function(tag) { return "FooBar "+tag+"!"; }""") // parameter
    .registerHelper("bar", """function() { return "Simple"; }""") // no param
    .registerHelper("hash", """function() { return "Hashed "+this.msg; }""") // parameter
    .registerHelper("hashObj", """function() { return "Hashed "+this.msg.heavy; }""") // object hash param
    .registerHelper("allTypes", """function() { return this.bool+" "+this.num+" "+this.nope+ " "+this.nada+ " "+this.s; }""") // object hash param
    .registerHelper("raw", """function() { return "<b>Hey</b>"; }""")
    .registerHelper("context", """function() { return this.player.name; }""")
    .registerHelper("noop", """function(options) { return options.fn(this); }""")
    .registerHelper("oneop", """function(options) { return this.small + "_"+options.fn(this)+"_"; }""")
    .registerHelper("strArg", """function(msg,options) { return msg + " _"+options.fn(this)+"_"; }""")

 */

/*
case class FooHelper() extends Helper {
  val args = List("tag")
  def eval(context: Context, expr: Expression)(implicit sb: Scalabars): StringWrapper = "FooBar "+ resolve("tag") + "!"
}
case class BarHelper() extends Helper {
  def eval(context: Context, expr: Expression)(implicit sb: Scalabars): StringWrapper = "Simple!"
}
case class HashHelper() extends Helper {
  def eval(context: Context, expr: Expression)(implicit sb: Scalabars): StringWrapper = "FooBar "+ resolve("this.msg") + "!"
}
case class HashObjHelper() extends Helper {
  def eval(context: Context, expr: Expression)(implicit sb: Scalabars): StringWrapper = "FooBar "+ resolve("this.desc.heavy") + "!"
}

 */
