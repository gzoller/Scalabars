package co.blocke.scalabars
package helpers.stock

import model._

/**
 * Do nothing.  This is called if a helper is unknown to the system.  It is intended users will override this helper
 * with their own desired behavior.
 */
case class HelperMissingHelper() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    ""
  }
}
