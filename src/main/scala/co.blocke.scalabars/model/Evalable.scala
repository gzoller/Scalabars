package co.blocke.scalabars
package model

/**
 * A thing can be evaluated to a Context
 */
trait Evalable {
  def eval(options: Options): EvalResult[_]
}