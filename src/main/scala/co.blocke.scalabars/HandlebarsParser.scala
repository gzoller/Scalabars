package co.blocke.scalabars

import fastparse._, SingleLineWhitespace._
import javax.script.ScriptEngine
import javax.script.ScriptEngineManager

case class HandlebarsParser() {

  private def template[_: P] = P(renderable.repX)

  private def renderable[_: P]: P[Renderable] = P(dynammicPartial | partial | section | inverted | strChars | !tryCloseBlock ~ variable)

  def section[_: P] = P(
    for {
      label <- openBlock
      block <- renderable.rep ~ closeBlock(label)
    } yield Section(label, block.toList)
  )
  def inverted[_: P] = P(
    for {
      label <- openNegBlock
      block <- renderable.rep ~ closeBlock(label)
    } yield Inverted(label, block.toList)
  )

  //  private def comment1[_: P] = P("{{!--" ~/ AnyChar.rep ~ "--}}").map(_ => Comment())
  //  private def comment2[_: P] = P("{{!" ~/ AnyChar.repX ~ "}}").map(_ => Comment())
  private def openBlock[_: P] = P("{{#" ~/ tag.! ~ "}}\n")
  private def openNegBlock[_: P] = P("{{^" ~/ tag.! ~ "}}\n")
  private def closeBlock[_: P](closeLabel: String) = P("{{/" ~/ closeLabel ~ "}}\n")
  private def tryCloseBlock[_: P] = P("{{/")

  private def partial[_: P] = P("{{>" ~/ tag.! ~ "}}").map(Partial(_, false))
  private def dynammicPartial[_: P] = P("{{>" ~ "(" ~ tag.! ~ ")" ~ "}}").map(Partial(_, true))

  private def variable[_: P] = P(unescapedVariable | escapedVariable)
  private def escapedVariable[_: P] = P("{{" ~/ tag.! ~ "}}").map(v => Variable(v, true))
  private def unescapedVariable[_: P] = P("{{{" ~/ tag.! ~ "}}}").map(v => Variable(v, false))
  private def tag[_: P]: P[String] = CharIn(""".,/,_,a-z,A-Z,0-9""").rep.!

  private def stringChars(c: Char) = c != '{'
  private def strChars[_: P] = P(CharsWhile(stringChars).!).map(Text(_))

  def compile(input: String): List[Renderable] = {
    parse(input, template(_)) match {
      case Parsed.Success(value, succesIndex) => value.toList
      case f @ Parsed.Failure(label, index, extra) =>
        println(f.trace())
        List.empty[Renderable]
      //        throw new Exception("Boom " + f.toString)
    }
  }

}
