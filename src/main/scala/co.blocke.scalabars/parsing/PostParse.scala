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
  def clean(seq: Seq[Renderable]): Seq[Renderable] = {
    val zipper = ListZipper(seq)
    while (!zipper.crashedRight) {
      if (zipper.focus.isDefined) zipper.focus.get match {
        case _: Whitespace =>
          zipper.mergeRightAs[Whitespace]((r1, r2) => Whitespace(r1.ws + r2.ws))

        case bh: BlockHelper => // This is guaranteed to have WS before & after
          val stage1 = bh.copy(body = bh.body.copy(body = clean(bh.body.body)))

          // Clip/Flush WS
          val stage2 = cleanOpenTag[BlockHelper](zipper, stage1.body)
          val stage3 = cleanCloseTag[BlockHelper](zipper, stage2.body)

          val stage4 = stage3.helper match {
            case ph: PartialHelper => stage3.copy(helper = ph.setParent(stage3))
            case _                 => stage3
          }
          zipper.modify(stage4)

        case bh: InlinePartialTag =>
          val stage1 = bh.copy(body = bh.body.copy(body = clean(bh.body.body)))
          zipper.modify(stage1)

          // Clip/Flush WS
          val stage2 = cleanOpenTag[InlinePartialTag](zipper, stage1.body)
          cleanCloseTag[InlinePartialTag](zipper, stage2.body)

        case ht: HelperTag =>
          val helper = ht.helper match {
            case ph: PartialHelper =>
              // Partial tags clip... non-partial, non-block tags don't
              val clearBefore = zipper
                .prevAs[Whitespace]
                .flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None)
                .isDefined
              val clearAfter = zipper
                .nextAs[Whitespace]
                .flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None)
                .isDefined
              val aloneOnLine = clearBefore && clearAfter

              var beforeFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None
              var afterFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None

              if (aloneOnLine) {
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
              ph.setParent(ht)

            case otherHelper =>
              if (ht.wsCtlBefore)
                flushOpenBefore(zipper)
              if (ht.wsCtlAfter)
                flushCloseAfter(zipper)
              otherHelper
          }
          zipper.modify(ht.copy(helper = helper))

        case _ => // do nothing
      }
      zipper.moveRight
    }
    zipper.toList
  }

  private def cleanOpenTag[T](z: ListZipper[Renderable], body: Block): T = {
    val clearAfter = body.body.head.asInstanceOf[Whitespace].ws.contains("\n")
    val clearBefore = z.index == 1 || z
      .prevAs[Whitespace]
      .flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None)
      .isDefined
    val aloneOnLine = clearBefore && clearAfter

    var beforeFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None
    var afterFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None

    if (aloneOnLine) {
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

  private def cleanCloseTag[T](z: ListZipper[Renderable], body: Block): T = {
    val clearBefore = body.body.last.asInstanceOf[Whitespace].ws.contains("\n")
    val clearAfter = z
      .nextAs[Whitespace]
      .flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None)
      .isDefined
    val aloneOnLine = clearBefore && clearAfter

    var beforeFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None
    var afterFn: Option[ListZipper[Renderable] => ListZipper[Renderable]] = None

    if (aloneOnLine) {
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

  private def clipOpenBefore(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.moveLeft
    if (z.index == 0 && !z.focus.get.asInstanceOf[Whitespace].ws.contains("\n"))
      z.delete
    else {
      z.modify(Whitespace(clipToLastNL(z.focus.get.asInstanceOf[Whitespace].ws)))
      z.moveRight
    }
    z
  }

  private def clipOpenAfter(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.focus.get match {
      case bh: BlockHelper =>
        val subzipper = ListZipper(bh.body.body)
        subzipper.modify(Whitespace(clipToNextNL(subzipper.focus.get.asInstanceOf[Whitespace].ws)))
        z.modify(bh.copy(body = bh.body.copy(body = subzipper.toList)))
      case ih: InlinePartialTag =>
        val subzipper = ListZipper(ih.body.body)
        subzipper.modify(Whitespace(clipToNextNL(subzipper.focus.get.asInstanceOf[Whitespace].ws)))
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

  private def clipCloseBefore(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.focus.get match {
      case bh: BlockHelper =>
        val subzipper = ListZipper(bh.body.body).last
        subzipper.modify(Whitespace(clipToLastNL(subzipper.focus.get.asInstanceOf[Whitespace].ws)))
        z.modify(bh.copy(body = bh.body.copy(body = subzipper.toList)))
      case ih: InlinePartialTag =>
        val subzipper = ListZipper(ih.body.body).last
        subzipper.modify(Whitespace(clipToLastNL(subzipper.focus.get.asInstanceOf[Whitespace].ws)))
        z.modify(ih.copy(body = ih.body.copy(body = subzipper.toList)))
    }
  }

  private def clipCloseAfter(z: ListZipper[Renderable]): ListZipper[Renderable] = {
    z.moveRight
    z.modify(Whitespace(clipToNextNL(z.focus.get.asInstanceOf[Whitespace].ws)))
    z.moveLeft
  }

  private def clipToNextNL(s: String): String =
    s.indexOf('\n') match {
      case -1 => s
      case i  => s.drop(i + 1)
    }

  private def clipToLastNL(s: String): String =
    s.lastIndexOf('\n') match {
      case -1 => s
      case i  => s.take(i + 1)
    }
}
