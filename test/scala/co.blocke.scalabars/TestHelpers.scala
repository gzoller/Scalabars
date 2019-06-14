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
  def run(expr: Expression)(implicit options: Options): StringWrapper = "Hashed " + options.hash("msg")
}

case class HashObjHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "Hashed " + options.hash("msg.heavy")
}

case class AllTypesHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    options.hash("bool") +
      " " + options.hash("num") +
      " " + options.hash("nope") +
      " " + options.hash("nada") +
      " " + options.hash("s")
}

case class RawHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = "<b>Hey</b>"
}

case class ContextHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    Handlebars.SafeString(resolve("this.player.name") + " -> " + resolve("player.name"))
}

case class NoopHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = options.fn(lookup("this"))
}

case class OneOpHelper() extends Helper() {
  def run(expr: Expression)(implicit options: Options): StringWrapper = resolve("this.small") + "_" + options.fn(lookup("this")) + "_"
}

case class StrArgHelper() extends Helper(List("msg")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = resolve("msg") + " _" + options.fn(lookup("this")) + "_"
}

