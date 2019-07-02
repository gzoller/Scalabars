package co.blocke.scalabars
package model

import renderables.Whitespace

case class RenderControl(
    opts:            Options,
    flushTrailingWS: Boolean       = false,
    accumulatedWS:   String        = "", // to accumulate leading ws
    out:             StringBuilder = new StringBuilder(),
    clippedWS:       String        = "" // for {{> tag}}  Need to save preceeding ws, even if clipped, to indent partial block
) {

  def addWS(ws: Whitespace): RenderControl =
    if (flushTrailingWS)
      this // ignore ws contribution if flushing...
    else
      this.copy(accumulatedWS = this.accumulatedWS + ws.ws, clippedWS = ws.clippedWS)

  // Add rendered Text element
  def addText(s: String): RenderControl =
    this.copy(
      accumulatedWS   = "",
      out             = this.out.append(this.accumulatedWS + s),
      flushTrailingWS = false,
      clippedWS       = "")

  /*
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
   */

  // Like addText but don't disturb clipping of trailing WS handling (used for adding rendered contents of a tag)
  def addContent(s: String): RenderControl = {
    this.copy(
      out           = this.out.append(this.accumulatedWS + s),
      accumulatedWS = "",
      clippedWS     = ""
    )
  }

  def reset(): RenderControl = this.copy(flushTrailingWS = false, clippedWS = "")
  def flushLeading(): RenderControl = this.copy(accumulatedWS = "")
  def flushTrailing(): RenderControl = this.copy(flushTrailingWS = true)
}
