package co.blocke.scalabars.model

trait SBLogger {
  def debug(msg: String)
  def info(msg: String)
  def warn(msg: String)
  def error(msg: String)
}

import java.util.logging.{ Logger, Level, Handler }

object JavaLogger {
  val logger = Logger.getAnonymousLogger()
}

case class JavaLogger(handler: Option[Handler] = None) extends SBLogger {
  handler.map(h => JavaLogger.logger.addHandler(h))
  def debug(msg: String) = JavaLogger.logger.log(Level.FINER, msg) // not output by default unless you configure logger
  def info(msg: String) = JavaLogger.logger.log(Level.INFO, msg)
  def warn(msg: String) = JavaLogger.logger.log(Level.WARNING, msg)
  def error(msg: String) = JavaLogger.logger.log(Level.SEVERE, msg)
}
