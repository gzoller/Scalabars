package co.blocke.scalabars

trait Expr
case class SimpleExpr(path: List[String]) extends Expr
case class FullExpr(label: String, args: List[Argument]) extends Expr
