package co.blocke.scalabars
package model

import renderables.HelperTag

trait Argument extends Evalable {
  def value: String
}

/**
 * Used for quoted strings
 * @param value
 */
case class StringArgument(value: String) extends Argument {
  def eval(options: Options): EvalResult[_] = StringEvalResult(value)
}

/**
 * Used for symbols/paths that can be resolved to a Context
 * @param unparsedPath
 */
case class PathArgument(unparsedPath: String) extends Argument {
  def value: String = unparsedPath
  def eval(options: Options): EvalResult[_] = ContextEvalResult(options.context.lookup(value))
}

/**
 * Assign a value to a given label, which is inserted into the local context
 * @param label
 * @param targetValue
 */
case class AssignmentArgument(label: String, targetValue: Argument) extends Argument {
  // $COVERAGE-OFF$These are never used in practice for AssignmentArguments but needed for trait
  def value: String = label + " = " + value.toString
  def eval(options: Options): EvalResult[_] = NoEvalResult() // Should Never Happen(tm)
  // $COVERAGE-ON$
}

/**
 * Expressions that evaluate to an Argument value (nested expression in place of usual argument)
 * @param expr
 */
case class ExpressionArgument(expr: HelperTag) extends Argument {
  def value: String = "(expression)"
  def eval(options: Options): EvalResult[_] = expr.eval(options)
}

/**
 * Expressions that evaluate to literals (null, undefined, numbers, true/false)
 * @param value
 */
case class LiteralArgument(value: String) extends Argument {
  def eval(options: Options): EvalResult[_] = value match {
    case "true"  => BooleanEvalResult(true)
    case "false" => BooleanEvalResult(false)
    case n if n.isNumeric =>
      BigDecimal(n) match {
        case d if d.isValidLong => LongEvalResult(d.toLongExact)
        case d                  => DoubleEvalResult(d.toDouble)
      }
    // $COVERAGE-OFF$Parser should protect us from this.  Left here as a safety
    case _ => NoEvalResult()
    // $COVERAGE-ON$
  }
}
