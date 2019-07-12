package co.blocke.scalabars.model

trait SBLogger {
  def debug(msg: String): Unit
  def info(msg: String): Unit
  def warn(msg: String): Unit
  def error(msg: String): Unit
}

import java.util.logging.{ Logger, Level, Handler }

object JavaLogger {
  val logger = Logger.getAnonymousLogger()
}

case class JavaLogger(handler: Option[Handler] = None) extends SBLogger {
  handler.map(h => JavaLogger.logger.addHandler(h))
  def debug(msg: String): Unit = JavaLogger.logger.log(Level.FINER, msg) // not output by default unless you configure logger
  def info(msg: String): Unit = JavaLogger.logger.log(Level.INFO, msg)
  def warn(msg: String): Unit = JavaLogger.logger.log(Level.WARNING, msg)
  def error(msg: String): Unit = JavaLogger.logger.log(Level.SEVERE, msg)
}
