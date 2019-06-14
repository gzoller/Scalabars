package co.blocke.scalabars
package helpers.stock

import model._
import org.json4s._

/**
 * Some helpers can return either a string or a context depending on use.  There are four uses cases to consider:
 *
 * 1. Simple/direct use:  {{foo}} -- Always resolves to String
 * 2. First-position sub-expression:  {{(foo) arg1 arg2}} -- Always resolves to String
 * 3. Use as an argument, string expected:  {{helper (foo) arg}} -- Resolves to String
 * 4. Use as an argument, context expected: {{helper (foo) arg}} -- Resolves to Context
 *
 * For lookup, #3 above would be if finding the lookup (thing+loc) results in a terminal node (i.e. scala value) then
 * return a string.  If the lookup results in a non-terminal node (array or object) a Context should be returned.
 * The trick is this:  How do you know which?  The caller would need to control this behavior, however the helper
 * itself must support the ability to resolve to either String or Context if it will have the ability to return
 * a Context.
 */
case class LookupHelper() extends Helper("thing", "loc") {

  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    (arg("thing") >> arg("loc")).getOrElse(
      throw new BarsException("lookup helper failed to resolve with arguments: " + rawArgValue("thing") + " and " + rawArgValue("loc"))
    )
  }
}