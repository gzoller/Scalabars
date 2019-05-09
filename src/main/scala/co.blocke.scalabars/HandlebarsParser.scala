package co.blocke.scalabars

import fastparse.ScalaWhitespace._
import fastparse._

case class HandlebarsParser() {

  private def template[_: P] = P(renderable.repX)
  private def renderable[_: P]: P[Renderable] = P(strChars | comment | block | inverted | unescapedExpr | escapedExpr)

  //  private def renderable[_: P]: P[Renderable] = P(strChars | comment | dynammicPartial | partial | block | inverted | !("{{/") ~ thing)

  private def escapedExpr[_: P] = P("{{" ~~ expr ~ "}}").map(_.copy(isEscaped = true))
  private def unescapedExpr[_: P] = P("{{{" ~~ expr ~ "}}}")

  private def openNegBlock[_: P] = P("{{^" ~/ path ~ "}}")
  private def openBlock[_: P] = P("{{#" ~/ expr ~ "}}")
  private def closeBlock[_: P](closeLabel: String) = P("{{/" ~/ closeLabel ~ "}}")

  //  private def partial[_: P] = P("{{>" ~/ label.! ~ "}}").map(Partial(_, false))
  //  private def dynammicPartial[_: P] = P("{{>" ~ "(" ~ label.! ~ ")" ~ "}}").map(Partial(_, true))

  private def expr[_: P] = P(path ~/ arg.rep).map { case (l, p, a) => SimpleExpression(l, p, a.toList) }
  private def arg[_: P]: P[Argument] = P(assignmentArg | stringArg | pathArg)
  private def stringArg[_: P] = P("\"" ~ CharsWhile(_ != '"').! ~ "\"" | "'" ~ CharsWhile(_ != '\'').! ~ "'").map(r => StringArgument(r))
  private def pathArg[_: P] = P(path).map(p => PathArgument(p._2))
  private def literalArg[_: P] = P("true" | "false" | "null" | "undefined" | "-".? ~~ CharsWhile(_.isDigit) ~~ ("." ~~ CharsWhile(_.isDigit)).?).!.map(r => StringArgument(r))
  private def assignmentArg[_: P] = P(label ~ "=" ~ P(literalArg | pathArg | stringArg)).map(r => AssignmentArgument(r._1, r._2))

  private def strChars[_: P] = P(CharsWhile(_ != '{').!).map(Text(_))
  private def comment[_: P] = P("{{!" ~/ CharPred(_ != '}').rep ~ "}}\n").map(_ => Comment())

  private def block[_: P] = P(
    for {
      expr <- openBlock
      contents <- renderable.repX ~ closeBlock(expr.label)
    } yield BlockExpression(expr.label, expr.path, expr.args, contents.toList)
  )
  private def inverted[_: P] = P(
    for {
      label <- openNegBlock
      block <- renderable.rep ~ closeBlock(label._1)
    } yield BlockExpression(label._1, label._2, List.empty[Argument], block.toList, true)
  )

  private def path[_: P] = P(element ~~ (separator ~~ element).repX)
    .map { case (f, pair) => ((f + pair.map(s => s._1 + s._2).mkString("")), f +: pair.map(_._2).toList) }
  private def element[_: P] = P(label | upDir | index | ".".! ~ &("/"))
  private def index[_: P] = P(("[" ~/ CharsWhile(_.isDigit).repX(1) ~ "]").!)
  private def separator[_: P] = P("/" | ".").!
  private def upDir[_: P] = P("..".!)
  private def label[_: P]: P[String] = CharsWhileIn("""_a-zA-Z0-9""").repX(1).!

  //-----------------------------
  def compile(input: String): List[Renderable] = {
    parse(input, template(_)) match {
      case Parsed.Success(value, _) => value.toList
      case f @ Parsed.Failure(label, index, extra) =>
        println(f.trace())
        List.empty[Renderable]
      //        throw new Exception("Boom " + f.toString)
    }
  }

  def pathCompile(input: String): Path = {
    parse(input, path(_)) match {
      case Parsed.Success(value, _)                => value._2
      case f @ Parsed.Failure(label, index, extra) => throw new BarsException("Path parsing failed for: " + f.toString)
    }
  }
}

/*
  "slurp until" in {
    def slurpUntil[_: P](delimiter: String) = P( ((AnyChar ~ !delimiter).rep ~ AnyChar).! ~ delimiter )
    parse(input = "RCD*hello~RCD2*world~", slurpUntil("~")(_)) should matchPattern {
      case Parsed.Success("RCD*hello", 10) =>
    }
  }

    "higher order slurp" in {
    def slurpUntil[_: P, T](delimiter: => P[T]) : P[_] =
      P(
        (((AnyChar ~ !delimiter).rep ~ AnyChar).! ~ delimiter)
      .map( { case (fields, _) => fields }) )

    def delimiter[_: P] = P ( "~" )
    def slurper[_: P] = slurpUntil(delimiter)

    val in = "RCD*hello~"
    val inLen = in.length
    parse(in, slurper(_)) should matchPattern {
      case Parsed.Success("RCD*hello", `inLen`) =>
    }
  }
 */ 