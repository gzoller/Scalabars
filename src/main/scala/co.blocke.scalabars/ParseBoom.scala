package co.blocke.scalabars

import fastparse.NoWhitespace._
import fastparse._

trait Stuff
case class Word(text: String) extends Stuff
case class WS(ws: String) extends Stuff

object ParseBoom extends App {

  private def template[_: P] = P(content.rep)
  private def content[_: P]: P[Stuff] = P(word | whitespace)

  def word(implicit ctx: P[_]): P[Word] = {
    val start = ctx.index

    if (start == ctx.input.length || ctx.input(start).isWhitespace)
      ctx.freshFailure()
    else {
      var i = start
      while (i < ctx.input.length && !ctx.input(i).isWhitespace)
        i += 1
      println("Word: " + ctx.input.slice(start, i))
      Thread.sleep(2000)
      ctx.freshSuccess(Word(ctx.input.slice(start, i)), i)
    }
  }

  def whitespace(implicit ctx: P[_]): P[WS] = {
    val start = ctx.index
    var i = start
    if (start == ctx.input.length)
      ctx.freshFailure()
    else {
      while (i < ctx.input.length && ctx.input(i).isWhitespace)
        i += 1
      println("WS: |" + ctx.input.slice(start, i) + "|")
      Thread.sleep(2000)
      ctx.freshSuccess(WS(ctx.input.slice(start, i)), i)
    }
  }

  def compile(input: String): Seq[Stuff] = {
    parse(input, template(_)) match {
      case Parsed.Success(value, _) =>
        println("Result: " + value)
        value
      case f @ Parsed.Failure(label, index, extra) =>
        throw new BarsException("Template parsing failed: " + f.toString)
    }
  }

  val input = "The quick brown fox"
  compile(input)
}
