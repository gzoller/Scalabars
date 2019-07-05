package co.blocke.scalabars
package model

trait FileGetter {
  // using the given path, retrieve the contents of the file
  def retrieveFileAtPath(path: String): String
}

case class NoopFileGetter() extends FileGetter {
  def retrieveFileAtPath(path: String): String = ""
}