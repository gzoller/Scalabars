package co.blocke.scalabars
package model

case class ParsedExpression(
    name:    String,
    args:    Seq[Argument]              = Seq.empty[Argument],
    exprArg: Option[ExpressionArgument] = None // Set when an embedded expression is in the 1st position of a tag (not an arg)
) {
  // Alternate constructor for sub-expressions
  def this(e: ExpressionArgument, args: Seq[Argument]) {
    this("__expr", args, Some(e))
  }
}