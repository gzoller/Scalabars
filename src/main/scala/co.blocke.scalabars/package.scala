package co.blocke

package object scalabars {

  type Path = List[String]

  implicit class OpsNum(val str: String) extends AnyVal {
    def isNumeric() = scala.util.Try(str.toDouble).isSuccess
  }

  implicit def string2Wrap(str: String): StringWrapper = RawStringWrapper(str)
}
