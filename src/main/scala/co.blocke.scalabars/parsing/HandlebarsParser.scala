package co.blocke.scalabars
package parsing

import model._
import renderables._
import fastparse.NoWhitespace._
import fastparse._
import co.blocke.listzipper.mutable.ListZipper

case class HandlebarsParser()(implicit val sb: Scalabars) {

  private def template[_: P] = P(renderable.rep)

  private def renderable[_: P]: P[Renderable] = P(tag | text)

  private def wsctl[_: P] = P("~").!
  private def ctl[_: P] = P("#*" | "#>" | CharIn("!^#>/&")).!

  // A very monadic tag parser! :-)
  private def tag[_: P] = P(
    for {

      // Parse these pieces: '{{ ~ #'
      initial <- (whitespace ~ "{".rep(min = 2, max = 4).! ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ ctl.?)
        .map {
          case (leadingWS, arity, wsCtlBefore, ctl) =>
            TagBuilder(leadingWS, arity.length, wsCtlBefore.isDefined, {
              if (arity.length == 4) Some("raw") else ctl
            })
        }

      // Parse expression (+args).  Could be something weird like a naked '^'
      stage2 <- initial.ctl match { // Break out #* so we can force "inline" to be the tag name
        case Some("#*") => P(spacesOrNL ~/ "inline" ~ spacesOrNL ~ arg).map { arg => initial.copy(expr = ParsedExpression("inline", Seq(arg))) }
        case Some("^") => (spacesOrNL ~/ expr.?).map { // expr is optional for {{^}}
          _ match { // ^ may be a poor-man's else--no expr
            case Some(e) => initial.copy(expr = e)
            case None    => initial.copy(expr = ParsedExpression("else"), ctl = None) // convert naked '^' into else
          }
        }
        case _ => (spacesOrNL ~ expr).map { expr => initial.copy(expr = expr) }
      }

      // Parse optional 'as' clause (block parameters) if normal/inverse block, then parse the rest of the tag
      stage3 <- stage2.ctl match { // Look for block tag and handle 'as' clause
        case Some("#") | Some("^") => (spacesOrNL ~/ asClause.? ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ "}".rep(min = stage2.arity, max = stage2.arity) ~ whitespace)
          .map {
            case (blockParams, wsCtlAfter, trailingWS) =>
              stage2.copy(blockParams = blockParams.getOrElse(Seq.empty[String]), wsCtlAfter = wsCtlAfter.isDefined, trailingWS = trailingWS)
          }
        case _ =>
          (spacesOrNL ~/ wsctl.? ~ spacesOrNL ~ "}".rep(min = stage2.arity, max = stage2.arity) ~ whitespace)
            .map {
              case (wsCtlAfter, trailingWS) => stage2.copy(wsCtlAfter = wsCtlAfter.isDefined, trailingWS = trailingWS)
            }
      }

      // Finally, pick up any tag body contents (if block tag)
      stage4 <- stage3.isBlock match {
        case true =>
          P((!(whitespace ~ closeBlock(stage3.arity, stage3.expr.name)) ~/ renderable).rep ~ whitespace ~ closeBlock(stage3.arity, stage3.expr.name) ~ whitespace)
            .map {
              case (contents, ws3, closeBlock, ws4) =>
                // Finalize any tags that may be inside block
                val finalContents = contents.foldLeft(Seq.empty[Renderable]) {
                  case (acc, item) =>
                    item match {
                      case tb: TagBuilder => acc ++ tb.finalize
                      case one            => acc :+ one
                    }
                }
                // rearrange body and whitespce (and wsCtl) for block (sew te block together to make it look/behave like a single, non-block tag)
                //                val wsCtlSave = closeBlock.wsCtlAfter
                val body = Block(
                  OpenTag(stage3.expr, stage3.wsCtlBefore, stage3.wsCtlAfter, stage3.leadingWS.ws.contains("\n"), stage3.arity),
                  stage3.trailingWS +: finalContents :+ ws3,
                  closeBlock
                )
                stage3.copy(body       = body, trailingWS = ws4)
            }
        case false => P("").map(_ => stage3) // pass thru... non-block tag
      }
    } yield stage4 match {
      case t if t.ctl.contains("&") => t.copy(arity = 3) // Replace & (no escape) with equivalent arity-3
      case _                        => stage4
    }
  )

  private def closeBlock[_: P](arity: Int, label: String) =
    P("{".rep(arity) ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ "/" ~ spacesOrNL ~ label ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ "}".rep(arity))
      .map { case (wcb, wca) => CloseTag(wcb.isDefined, wca.isDefined, false, arity) }

  private def spaces[_: P] = P(CharsWhileIn(" \t", 0))
  private def spacesOrNL[_: P] = P(CharsWhileIn(" \t\n", 0))
  private def leadingSpace[_: P] = P(CharsWhileIn(" \t\n", 0)).!
  private def trailingSpace[_: P] = P(spaces ~ "\n".?).!

  private def expr[_: P] = P(subExpr | simpleExpr)
  private def subExpr[_: P] = P(exprArg ~ arg.rep).map { case (e, a) => new ParsedExpression(e, a.toList) }
  private def simpleExpr[_: P] = P(unparsedPath ~ spacesOrNL ~ (arg ~ spacesOrNL).rep).map { case (up, a) => ParsedExpression(up, a.toList) }
  private def asClause[_: P] = P("as" ~ spacesOrNL ~ "|" ~ spacesOrNL ~ (label ~ spacesOrNL).rep ~ "|")

  private def arg[_: P]: P[Argument] = P(assignmentArg | stringArg | literal | pathArg | exprArg)
  private def literal[_: P] = P("true" | "false" | "null" | "undefined" | number).!.map(r => LiteralArgument(r))
  private def number[_: P] = P(("-".? ~~ CharsWhileIn("0-9") ~~ ("." ~~ CharsWhileIn("0-9")).?).!)
  private def stringArg[_: P] = P("\"" ~~ CharsWhile(_ != '"').! ~~ "\"" | "'" ~~ CharsWhile(_ != '\'').! ~~ "'").map(r => StringArgument(r))
  private def pathArg[_: P] = P(!"as" ~ unparsedPath).map(p => PathArgument(p))
  private def exprArg[_: P] = P("(" ~ spacesOrNL ~ simpleExpr ~ spacesOrNL ~ ")")
    .map(e => ExpressionArgument(TagBuilder(Whitespace(""), 3, expr = e, trailingWS = Whitespace("")).finalize.apply(1).asInstanceOf[HelperTag]))

  private def literalArg[_: P] = P("true" | "false" | "null" | "undefined" | "-".? ~~ CharsWhile(_.isDigit) ~~ ("." ~~ CharsWhile(_.isDigit)).?).!.map(r => StringArgument(r))
  private def assignmentArg[_: P] = P(label ~ "=" ~ P(literalArg | pathArg | stringArg)).map(r => AssignmentArgument(r._1, r._2))
  private def label[_: P]: P[String] = CharsWhileIn("""_a-zA-Z0-9""").repX(1).!
  private def unparsedPath[_: P] = P("@partial-block".! | _unparsedPath)
  private def _unparsedPath[_: P] = P("@".?.! ~~ CharsWhileIn("""_a-zA-Z0-9./[]""").repX(1).!).map(s => s._1 + s._2)

  // Grab next text string (less any trailing whitespace) until either end-of-input or '{{'
  def text(implicit ctx: P[_]): P[Text] = {
    val start = ctx.index

    if (start >= ctx.input.length - 2)
      ctx.freshFailure()
    else {
      // Returns: (pos, end of input?)
      def findDoubleBrace(idx: Int): Option[Int] =
        findSingleBrace(idx).flatMap { b =>
          findSingleBrace(b + 1) match {
            case Some(b2) if b2 == b + 1 => Some(b)
            case Some(b2)                => findDoubleBrace(b2)
            case None                    => None // end-of-input reached, no double breace
          }
        }

      def findSingleBrace(idx: Int): Option[Int] = {
        var i = idx
        while (i < ctx.input.length && ctx.input(i) != '{')
          i += 1
        if (i == ctx.input.length) // end of input
          None
        else
          Some(i)
      }

      findDoubleBrace(start) match {
        case Some(p) =>
          // Now backtrack over any whitespace
          var k = p - 1
          while (k >= 0 && k >= start && ctx.input(k).isWhitespace)
            k -= 1
          if (k < 0)
            ctx.freshFailure()
          else
            ctx.freshSuccess(Text(ctx.input.slice(start, Math.max(0, k) + 1)), k + 1)
        case None =>
          ctx.freshSuccess(Text(ctx.input.slice(start, ctx.input.length)), ctx.input.length) // take rest of string...no "{{" found
      }
    }
  }

  // Grab whitespace until end-of-input or '{{'
  // May be empty, but always succeeds
  def whitespace(implicit ctx: P[_]): P[Whitespace] = {
    val start = ctx.index
    var i = start
    while (i < ctx.input.length && ctx.input(i).isWhitespace)
      i += 1
    if (i == start)
      ctx.freshSuccess(Whitespace(""))
    else
      ctx.freshSuccess(Whitespace(ctx.input.slice(start, i)), i)
  }

  //-----------------------------<< Compile!

  def compile(input: String): Seq[Renderable] = {
    parse(input, template(_)) match {
      case Parsed.Success(value, _) => {
        val set = value.foldLeft(List.empty[Renderable]) {
          case (set, item) =>
            item match {
              case _: Text       => set :+ item
              case t: TagBuilder => set ++ t.finalize
            }
        }
        clean(set)
      }
      case f @ Parsed.Failure(label, index, extra) =>
        throw new BarsException("Template parsing failed: " + f.toString)
    }
  }

  private def clean(seq: Seq[Renderable]): Seq[Renderable] = {
    val zipper = ListZipper(seq)
    while (!zipper.crashedRight) {
      if (zipper.focus.isDefined) zipper.focus.get match {
        case _: Whitespace =>
          zipper.mergeRightAs[Whitespace]((r1, r2) => Whitespace(r1.ws + r2.ws))

        case bh: BlockHelper => // This is guaranteed to have WS before & after
          val stage1 = bh.copy(body = bh.body.copy(body = clean(bh.body.body)))

          // Open tag alone on line?
          val clearAfter = stage1.body.body.head.asInstanceOf[Whitespace].ws.contains("\n")
          val clearBefore = zipper.index == 1 || zipper.prevAs[Whitespace].flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None).isDefined
          val stage2 = stage1.copy(body = stage1.body.copy(openTag = stage1.body.openTag.copy(aloneOnLine = clearBefore && clearAfter)))

          // Close tag alone on line?
          val clearBefore2 = stage2.body.body.last.asInstanceOf[Whitespace].ws.contains("\n")
          val clearAfter2 = zipper.nextAs[Whitespace].flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None).isDefined
          val stage3 = stage2.copy(body = stage2.body.copy(closeTag = stage2.body.closeTag.copy(aloneOnLine = clearBefore2 && clearAfter2)))
          zipper.modify(stage3)

        case bh: InlinePartialTag =>
          val stage1 = bh.copy(body = bh.body.copy(body = clean(bh.body.body)))

          // Open tag alone on line?
          val clearAfter = stage1.body.body.head.asInstanceOf[Whitespace].ws.contains("\n")
          val clearBefore = zipper.index == 1 || zipper.prevAs[Whitespace].flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None).isDefined
          val stage2 = stage1.copy(body = stage1.body.copy(openTag = stage1.body.openTag.copy(aloneOnLine = clearBefore && clearAfter)))

          // Close tag alone on line?
          val clearBefore2 = stage2.body.body.last.asInstanceOf[Whitespace].ws.contains("\n")
          val clearAfter2 = zipper.nextAs[Whitespace].flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None).isDefined
          val stage3 = stage2.copy(body = stage2.body.copy(closeTag = stage2.body.closeTag.copy(aloneOnLine = clearBefore2 && clearAfter2)))
          zipper.modify(stage3)

        case ht: HelperTag =>
          val clearBefore = zipper.prevAs[Whitespace].flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None).isDefined
          val clearAfter = zipper.nextAs[Whitespace].flatMap(ws => if (ws.ws.contains("\n")) Some(ws) else None).isDefined
          zipper.modify(ht.copy(aloneOnLine = clearBefore && clearAfter))

        case _ => // do nothing
      }
      zipper.moveRight
    }
    zipper.toList
  }
}