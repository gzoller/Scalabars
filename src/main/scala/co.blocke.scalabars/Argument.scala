package co.blocke.scalabars

trait Argument

case class StringArgument(value: String) extends Argument

case class PathArgument(path: List[String]) extends Argument

case class AssignmentArgument(label: String, value: Argument) extends Argument
