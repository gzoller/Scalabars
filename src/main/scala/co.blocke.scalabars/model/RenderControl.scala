package co.blocke.scalabars
package model

import renderables.Whitespace

case class RenderControl(
    opts:          Options,
    accumulatedWS: String        = "", // to accumulate leading ws
    out:           StringBuilder = new StringBuilder(),
    clippedWS:     String        = "" // for {{> tag}}  Need to save preceeding ws, even if clipped, to indent partial block
) {

  def addWS(ws: Whitespace): RenderControl = this.copy(accumulatedWS = this.accumulatedWS + ws.ws, clippedWS = ws.clippedWS)

  // Add rendered Text element
  def addText(s: String): RenderControl =
    this.copy(accumulatedWS = "", out = this.out.append(this.accumulatedWS + s), clippedWS = "")
}
