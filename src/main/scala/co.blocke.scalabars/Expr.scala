package co.blocke.scalabars

trait Expr { val label: String }
case class SimpleExpr(label: String, path: List[String]) extends Expr
case class FullExpr(label: String, args: List[Argument]) extends Expr
