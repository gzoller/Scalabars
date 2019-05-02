package co.blocke.scalabars

import fastparse._, NoWhitespace._

case class PathParser() {

  private def path[_: P] = P(element ~ (separator ~ element).rep).map(c => List(c._1) ++ c._2)

  private def element[_: P] = P(label | upDir | index)
  private def index[_: P] = P(("[" ~/ CharIn("0-9").rep(1)).! ~ "]")
  private def separator[_: P] = P("/" | ".")
  private def upDir[_: P] = P("..".!)
  private def label[_: P]: P[String] = CharIn("""_,a-z,A-Z,0-9""").rep(1).!

  def unpackPath(p: String): List[String] = parse(p, path(_)) match {
    case Parsed.Success(value, _) => value
    case _                        => throw new Exception("Boom")
  }
}
