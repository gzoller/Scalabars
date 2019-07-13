package co.blocke.scalabars
package parsing

import model._
import renderables._
import co.blocke.listzipper.mutable.ListZipper

/**
  * A whole lot of cleaning up:
  *   * Remove double WS that can happen when 2 tags are next to each other (each tag is guaranteed to have a before/after WS at parse time)
  *   * Determine if tags are alone on their line
  *   * If a tag is alone on a line, pre-clip WS before/after the tag
  *   * Wire parent for PartialHelper
  */
object PostParse {
  def clean(seq: Seq[Renderable])(implicit compileOptions: Map[String, Boolean]): Seq[Renderable] = {
    val zipper = ListZipper(seq)
    while (!zipper.crashedRight) {
      if (zipper.focus.isDefined) zipper.focus.get match {

        case bh: BlockHelper =>
          val stage1 = cleanOpenTag[BlockHelper](zipper, bh.body)

          val stage2 = stage1.copy(body = stage1.body.copy(body = clean(stage1.body.body)))
          zipper.modify(stage2)

          val stage3 = cleanCloseTag[BlockHelper](zipper, stage2.body)

          val stage4 = stage3.helper match {
            case ph: PartialHelper => stage3.copy(helper = ph.setParent(stage3))
            case _                 => stage3
          }
          zipper.modify(stage4)

        case ipTag: InlinePartialTag =>
          val stage1 = cleanOpenTag[InlinePartialTag](zipper, ipTag.body)
          val stage2 = stage1.copy(body = stage1.body.copy(body = clean(stage1.body.body)))
          zipper.modify(stage2)
          cleanCloseTag[InlinePartialTag](zipper, stage2.body)

        case ht: HelperTag =>
          val (helper, alone) = ht.helper match {
            case ph: PartialHelper =>
              // Partial tags clip... non-partial, non-block tags don't
              val clearBefore = zipper.index == 0 || (zipper.prevAs[Whitespace] match {
                case Some(ws) if zipper.index == 1 || ws.ws.contains("\n") || ws.isClipped => true
                case _                                                                     => false
              })
              val clearAfter = zipper.nextAs[Whitespace] match {
                case Some(ws) if ws.ws.contains("\n") || ws.isClipped => true
                case _                                                => false
              }
              val aloneOnLine = clearBefore && clearAfter

              var beforeFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None
              var afterFn: Option[ListZipper[Renderable] => ListZipper[Renderable]]  = None

              if (aloneOnLine && !compileOptions.getOrElse("ignoreStandalone", false)) {
                beforeFn = Some(clipOpenBefore)
                afterFn = Some(clipCloseAfter)
              }
              if (ht.wsCtlBefore) {
                beforeFn = Some(flushOpenBefore)
              }
              if (ht.wsCtlAfter)
                afterFn = Some(flushCloseAfter)

              List(beforeFn, afterFn)
                .foldLeft(zipper) { case (accZ, fn) => fn.map(_(accZ)).getOrElse(accZ) }
                .focus
                .get
                .asInstanceOf[HelperTag]
              (ph.setParent(ht), aloneOnLine)

            case otherHelper =>
              if (ht.wsCtlBefore)
                flushOpenBefore(zipper)
              if (ht.wsCtlAfter)
                flushCloseAfter(zipper)
              (otherHelper, false)
          }
          zipper.modify(ht.copy(helper = helper, aloneOnLine = alone))

        case _ => // do nothing
      }
      zipper.moveRight
    }
    zipper.toList
  }

  private def cleanOpenTag[T](z: ListZipper[Renderable], body: Block)(implicit compileOptions: Map[String, Boolean]): T = {
    val clearBefore = z.index == 0 || (z.prevAs[Whitespace] match {
      case Some(ws) if z.index == 1 || ws.ws.contains("\n") || ws.isClipped => true
      case _                                                                => false
    })
    val clearAfter = body.body.head match {
      case ws: Whitespace if ws.ws.contains("\n") || ws.isClipped =>
        true // first ws in body MUST have \n
      case _ => false
    }
    val aloneOnLine = clearBefore && clearAfter

    var beforeFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None
    var afterFn: Option[ListZipper[Renderable] => ListZipper[Renderable]]  = None

    if (aloneOnLine && !compileOptions.getOrElse("ignoreStandalone", false)) {
      beforeFn = Some(clipOpenBefore)
      afterFn = Some(clipOpenAfter)
    }
    if (body.openTag.wsCtlBefore)
      beforeFn = Some(flushOpenBefore)
    if (body.openTag.wsCtlAfter)
      afterFn = Some(flushOpenAfter)

    List(beforeFn, afterFn)
      .foldLeft(z) { case (accZ, fn) => fn.map(_(accZ)).getOrElse(accZ) }
      .focus
      .get
      .asInstanceOf[T]
  }

  private def cleanCloseTag[T](z: ListZipper[Renderable], body: Block)(implicit compileOptions: Map[String, Boolean]): T = {
    val clearBefore = body.body.last match {
      case ws: Whitespace if ws.ws.contains("\n") || ws.isClipped => true
      case _                                                      => false
    }
    val clearAfter = z.nextAs[Whitespace] match {
      case Some(ws) if ws.ws.contains("\n") || ws.isClipped => true
      case _                                                => false
    }
    val aloneOnLine = clearBefore && clearAfter

    var beforeFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None
    var afterFn: Option[ListZipper[Renderable] => ListZipper[Renderable]]  = None

    if (aloneOnLine && !compileOptions.getOrElse("ignoreStandalone", false)) {
      beforeFn = Some(clipCloseBefore)
      afterFn = Some(clipCloseAfter)
    }
    if (body.closeTag.wsCtlBefore)
      beforeFn = Some(flushCloseBefore)
    if (body.closeTag.wsCtlAfter)
      afterFn = Some(flushCloseAfter)

    List(beforeFn, afterFn)
      .foldLeft(z) { case (accZ, fn) => fn.map(_(accZ)).getOrElse(accZ) }
      .focus
      .get
      .asInstanceOf[T]
  }

  private def flushOpenBefore[T](z: ListZipper[Renderable]): ListZipper[Renderable] = {
    val save = z.index
    z.moveLeft
    while (!z.crashedLeft && z.focus.get.isInstanceOf[Whitespace]) {
      z.delete
      z.moveLeft
    }
    if (z.index != save)
      z.moveRight
    z
  }

  private def flushOpenAfter(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.focus.get match {
      case bh: BlockHelper =>
        val subzipper = ListZipper(bh.body.body)
        while (!subzipper.isEmpty && subzipper.focus.get.isInstanceOf[Whitespace]) subzipper.delete
        z.modify(bh.copy(body = bh.body.copy(body = subzipper.toList)))
      case ih: InlinePartialTag =>
        val subzipper = ListZipper(ih.body.body)
        while (!subzipper.isEmpty && subzipper.focus.get.isInstanceOf[Whitespace]) subzipper.delete
        z.modify(ih.copy(body = ih.body.copy(body = subzipper.toList)))
    }
  }

  private def flushCloseBefore(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.focus.get match {
      case bh: BlockHelper =>
        val subzipper = ListZipper(bh.body.body).last
        while (!subzipper.isEmpty && subzipper.focus.get.isInstanceOf[Whitespace]) subzipper.delete
        z.modify(bh.copy(body = bh.body.copy(body = subzipper.toList)))
      case ih: InlinePartialTag =>
        val subzipper = ListZipper(ih.body.body).last
        while (!subzipper.isEmpty && subzipper.focus.get.isInstanceOf[Whitespace]) subzipper.delete
        z.modify(ih.copy(body = ih.body.copy(body = subzipper.toList)))
    }
  }

  private def flushCloseAfter(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    val save = z.index
    z.moveRight
    while (!z.isEmpty && z.focus.get.isInstanceOf[Whitespace]) z.delete
    if (z.index != save)
      z.moveLeft
    z
  }

  private def clipOpenBefore(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.prevAs[Whitespace] match {
      case Some(ws) =>
        z.moveLeft
        if (ws.ws.contains("\n")) {
          // We care about and keep ws clipped going backwards!
          val (trimmed, cut, isClipped) = clipToLastNL(z.focusAs[Whitespace].get.ws)
          z.modify(Whitespace(trimmed, cut, ws.isClipped | isClipped)) // create new Whitespace is ok here
        } else
          z.modify(Whitespace("", ws.ws, true)) // alone on line, no \n, so just zero it out w/clip saving
        z.moveRight
      case None => // do nothing
    }
    z
  }

  private def clipOpenAfter(z: ListZipper[Renderable]): ListZipper[Renderable] = { // ws inside body
    z.focus.get match {
      case bh: BlockHelper =>
        val subzipper            = ListZipper(bh.body.body)
        val ws                   = subzipper.focusAs[Whitespace].get
        val (trimmed, isClipped) = clipToNextNL(ws.ws)
        subzipper.modify(ws.copy(ws = trimmed, isClipped = ws.isClipped | isClipped)) // copy--don't create new Whitespace
        z.modify(bh.copy(body = bh.body.copy(body = subzipper.toList)))

      case ih: InlinePartialTag =>
        val subzipper            = ListZipper(ih.body.body)
        val ws                   = subzipper.focusAs[Whitespace].get
        val (trimmed, isClipped) = clipToNextNL(ws.ws)
        subzipper.modify(ws.copy(ws = trimmed, isClipped = ws.isClipped | isClipped))
        z.modify(ih.copy(body = ih.body.copy(body = subzipper.toList)))
    }
  }

  private def clipCloseBefore(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.focus.get match {
      case bh: BlockHelper =>
        val subzipper = ListZipper(bh.body.body).last
        val ws        = subzipper.focusAs[Whitespace].get
        if (!ws.ws.contains("\n") && ws.isClipped)
          // Already clipped, and we can't be in this function unless tag is alone-on-line, so just eliminate the remaining ws
          subzipper.delete
        else {
          val (trimmed, cut, isClipped) = clipToLastNL(ws.ws)
          subzipper.modify(Whitespace(trimmed, cut, isClipped))
        }
        z.modify(bh.copy(body = bh.body.copy(body = subzipper.toList)))

      case ih: InlinePartialTag =>
        val subzipper                 = ListZipper(ih.body.body).last
        val (trimmed, cut, isClipped) = clipToLastNL(subzipper.focus.get.asInstanceOf[Whitespace].ws)
        subzipper.modify(Whitespace(trimmed, cut, isClipped))
        z.modify(ih.copy(body = ih.body.copy(body = subzipper.toList)))
    }
  }

  private def clipCloseAfter(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.moveRight
    val ws                   = z.focusAs[Whitespace].get
    val (trimmed, isClipped) = clipToNextNL(ws.ws)
    z.modify(ws.copy(ws = trimmed, isClipped = ws.isClipped | isClipped))
    z.moveLeft
  }

  private def clipToNextNL(s: String): (String, Boolean) = // Return (fixed string, clipped part minus newline)
    s.indexOf('\n') match {
      // $COVERAGE-OFF$Never seems to be triggered---left in as a safety
      case -1 => (s, false)
      // $COVERAGE-ON$
      case i =>
        (s.drop(i + 1), true)
    }

  private def clipToLastNL(s: String): (String, String, Boolean) = // Return (fixed string, clipped part minus newline)
    s.lastIndexOf('\n') match {
      // $COVERAGE-OFF$Never seems to be triggered---left in as a safety
      case -1 => (s, "", false)
      // $COVERAGE-ON$
      case i =>
        val fixed = s.take(i + 1)
        (fixed, s.diff(fixed), true)
    }
}
