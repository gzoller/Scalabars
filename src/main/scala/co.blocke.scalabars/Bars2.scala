package co.blocke.scalabars

import fastparse._, ScalaWhitespace._

case class Bars2() {

  private def template[_: P] = P(renderable.repX)
  private def renderable[_: P]: P[Renderable] = P(strChars | comment | block | inverted | !("{{/") ~ thing)
  //  private def renderable[_: P]: P[Renderable] = P(strChars | comment | dynammicPartial | partial | block | inverted | !("{{/") ~ thing)

  private def thing[_: P] = P(unescapedThing | escapedThing)
  private def escapedThing[_: P] = P("{{" ~/ expr ~ "}}").map(Thing(_, true))
  private def unescapedThing[_: P] = P("{{{" ~/ expr ~ "}}}").map(Thing(_, false))

  private def openNegBlock[_: P] = P("{{^" ~/ label.! ~ "}}\n")
  private def openBlock[_: P] = P("{{#" ~/ label.rep ~ "}}").map(_.toList)
  private def closeBlock[_: P](closeLabel: String) = P("{{/" ~/ closeLabel ~ "}}\n")

  //  private def partial[_: P] = P("{{>" ~/ label.! ~ "}}").map(Partial(_, false))
  //  private def dynammicPartial[_: P] = P("{{>" ~ "(" ~ label.! ~ ")" ~ "}}").map(Partial(_, true))

  private def expr[_: P] = fullExpr | simpleExpr
  private def simpleExpr[_: P] = P(path).map(r => SimpleExpr(r))
  private def fullExpr[_: P] = P(label ~ arg.rep(1)).map(r => FullExpr(r._1, r._2.toList))
  private def arg[_: P]: P[Argument] = P(assignmentArg | stringArg | pathArg)
  private def stringArg[_: P] = P("\"" ~ CharsWhile(_ != '"').! ~ "\"" | "'" ~ CharsWhile(_ != '\'').! ~ "'").map(r => StringArgument(r))
  private def pathArg[_: P] = P(path).map(p => PathArgument(p))
  private def literalArg[_: P] = P("true" | "false" | "null" | "undefined" | "-".? ~~ CharsWhile(_.isDigit) ~~ ("." ~~ CharsWhile(_.isDigit)).?).!.map(r => StringArgument(r))
  private def assignmentArg[_: P] = P(label ~ "=" ~ P(literalArg | pathArg | stringArg)).map(r => AssignmentArgument(r._1, r._2))

  private def strChars[_: P] = P(CharsWhile(_ != '{').!).map(Text(_))
  private def comment[_: P] = P("{{!" ~/ CharPred(_ != '}').rep ~ "}}\n").map(_ => Comment())

  private def block[_: P] = P(
    for {
      args <- openBlock
      block <- renderable.repX ~ closeBlock(args.head)
    } yield HelperOrSection(args.head, args.tail, block.toList)
  )

  //_ - every 10 yrs _ conducts an enumeration of the population
  def inverted[_: P] = P(
    for {
      label <- openNegBlock
      block <- renderable.rep ~ closeBlock(label)
    } yield Inverted(label, block.toList)
  )

  private def path[_: P] = P(element ~~ (separator ~~ element).repX).map(c => List(c._1) ++ c._2)
  private def element[_: P] = P(label | upDir | index)
  private def index[_: P] = P(("[" ~/ CharsWhile(_.isDigit).repX(1)).! ~ "]")
  private def separator[_: P] = P("/" | ".")
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
}
