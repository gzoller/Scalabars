package co.blocke.scalabars
package parsing

import fastparse.ScalaWhitespace._
import fastparse._

case class PathParser() {

  implicit val LabelSpaces = """([$_a-zA-Z0-9 ])""".r

  private def path[_: P] = P(element ~~ (separator ~~ element).repX)
    .map { case (f, pair) => (f + pair.map(s => s._1 + s._2).mkString(""), f +: pair.map(_._2).toList) }
  private def element[_: P] = P(specialVar | indexElement | normalElement)
  private def indexElement[_: P] = P(CharsWhileIn("""0-9""").repX(1) | "[" ~~ CharsWhileIn("""0-9""").repX(1) ~~ "]").!
  private def specialVar[_: P] = P("@".! ~~ CharsWhileIn("""_a-zA-Z0-9\-""").repX(1).!).map(s => s._1 + s._2)
  private def normalElement[_: P] = P(upDir | ".".! ~~ &("/") | ".".! | LabelWithSpaces | index)
  private def index[_: P] = P(("[" ~/ CharsWhile(_.isDigit).repX(1) ~ "]").!)
  private def separator[_: P] = P("/" | ".").!
  private def upDir[_: P] = P("..".!)

  def LabelWithSpaces(implicit ctx: P[_]): P[String] = {
    var index = ctx.index
    val input = ctx.input
    val start = index
    var done = false

    while (!done && index < input.length)
      input(index) match {
        case LabelSpaces(x) => index += 1
        case _              => done = true
      }
    // TODO: Not sure this is needed.  @root determination should be made in Context
    //    if (index == start)
    //      ctx.freshSuccess("@root", index)
    //    else
    ctx.freshSuccess(input.slice(start, index), index)
  }

  //-----------------------------
  def pathCompile(input: String): Path = {
    parse(input, path(_)) match {
      case Parsed.Success(value, _)                => value._2
      case f @ Parsed.Failure(label, index, extra) => throw new BarsException("Path parsing failed: " + f.toString)
    }
  }
}