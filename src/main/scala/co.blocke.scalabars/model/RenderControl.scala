package co.blocke.scalabars
package model

//TODO: Remove clipTrailingWS from RC

case class RenderControl(
    opts:            Options,
    clipTrailingWS:  Boolean       = false,
    flushTrailingWS: Boolean       = false,
    accumulatedWS:   String        = "", // to accumulate leading ws
    out:             StringBuilder = new StringBuilder()
) {

  def addWS(ws: String): RenderControl =
    if (flushTrailingWS || ws.isEmpty)
      this // ignore ws contribution if flushing...
    else
      this.copy(accumulatedWS = this.accumulatedWS + ws)

  // Add rendered Text element
  def addText(s: String): RenderControl =
    this.copy(
      accumulatedWS   = "",
      out             = this.out.append(this.accumulatedWS + s),
      clipTrailingWS  = false,
      flushTrailingWS = false)

  def ws(): String = {
    accumulatedWS.foldLeft("|") {
      case (acc, c) =>
        c match {
          case '\n' => acc + """\n"""
          case '\t' => acc + """\t"""
          case ' '  => acc + """."""
        }
    } + "|"
  }

  // Like addText but don't disturb clipping of trailing WS handling (used for adding rendered contents of a tag)
  def addContent(s: String): RenderControl = {
    this.copy(
      out           = this.out.append(this.accumulatedWS + s),
      accumulatedWS = ""
    )
  }

  def reset(): RenderControl = this.copy(clipTrailingWS  = false, flushTrailingWS = false)
  def flushLeading(): RenderControl = this.copy(accumulatedWS = "")
  def flushTrailing(): RenderControl = this.copy(flushTrailingWS = true)
  /*
  def clipLeading(): RenderControl = this.copy(accumulatedWS = clipToLastNL(this.accumulatedWS))
  def clipTrailing(): RenderControl = this.copy(clipTrailingWS = true)

  private def clipToNextNL(s: String): String = {
    s.indexOf('\n') match {
      case -1 => s
      case i  => s.drop(i + 1)
    }
  }
  private def clipToLastNL(s: String): String = {
    val after = s.lastIndexOf('\n') match {
      case -1 => s
      case i  => s.take(i + 1)
    }
    after
  }*/
}
