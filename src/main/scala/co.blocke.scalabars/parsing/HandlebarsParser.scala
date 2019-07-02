package co.blocke.scalabars
package parsing

import model._
import renderables._
import fastparse.NoWhitespace._
import fastparse._

case class HandlebarsParser()(implicit val sb: Scalabars) {

  //-----------------------------<< Top-Level Parser
  private def template[_: P] = P(whitespace.? ~ renderable.rep).map {
    case (ws, r) => ws.map(w => w +: r.flatten).getOrElse(r.flatten)
  }
  private def renderable[_: P]: P[Seq[Renderable]] = P(comment | tag | text)

  private def comment[_: P] = P(newComment | oldComment)
  private def oldComment[_: P] =
    P("{{!--" ~ (!"--}}" ~ AnyChar).rep.! ~ "--}}" ~ whitespace.?).map {
      case (cmt, ws) =>
        if (ws.isDefined) Seq(Comment(cmt), Whitespace(ws.get.ws)) else Seq(Comment(cmt))
    }
  private def newComment[_: P] =
    P("{{!" ~ (!"}}" ~ AnyChar).rep.! ~ "}}" ~ whitespace.?)
      .map {
        case (cmt, ws) =>
          if (ws.isDefined) Seq(Comment(cmt), Whitespace(ws.get.ws)) else Seq(Comment(cmt))
      }

  //-----------------------------<< Tag Parser
  // A very monadic tag parser! :-)
  private def tag[_: P] =
    P(
      for {

        // Parse these pieces: '{{ ~ #'
        initial <- ("{".rep(min = 2, max = 4).!
          ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ ctl.?)
          .map {
            case (arity, wsCtlBefore, ctl) =>
              TagBuilder(arity.length, wsCtlBefore.isDefined, {
                if (arity.length == 4) Some("raw") else ctl
              })
          }

        // Parse expression (+args).  Could be something weird like a naked '^'
        stage2 <- initial.ctl match { // Break out #* so we can force "inline" to be the tag name
          case Some("#*") =>
            P(spacesOrNL ~/ "inline" ~ spacesOrNL ~ arg).map { arg =>
              initial.copy(expr = ParsedExpression("inline", Seq(arg)))
            }
          case Some("^") =>
            (spacesOrNL ~/ expr.?).map { // expr is optional for {{^}}
              _ match { // ^ may be a poor-man's else--no expr
                case Some(e) => initial.copy(expr = e)
                case None =>
                  initial
                    .copy(expr = ParsedExpression("else"), ctl = None) // convert naked '^' into else
              }
            }
          case _ =>
            (spacesOrNL ~ expr).map { expr =>
              initial.copy(expr = expr)
            }
        }

        // Parse optional 'as' clause (block parameters) if normal/inverse block, then parse the rest of the tag
        stage3 <- stage2.ctl match { // Look for block tag and handle 'as' clause
          case Some("#") | Some("^") =>
            (spacesOrNL ~/ asClause.? ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ "}"
              .rep(min = stage2.arity, max = stage2.arity) ~ whitespace.?)
              .map {
                case (blockParams, wsCtlAfter, trailingWS) =>
                  stage2.copy(
                    blockParams = blockParams.getOrElse(Seq.empty[String]),
                    wsCtlAfter  = wsCtlAfter.isDefined,
                    trailingWS  = trailingWS)
              }
          case _ =>
            (spacesOrNL ~/ wsctl.? ~ spacesOrNL ~ "}"
              .rep(min = stage2.arity, max = stage2.arity) ~ whitespace.?)
              .map {
                case (wsCtlAfter, trailingWS) =>
                  stage2.copy(wsCtlAfter = wsCtlAfter.isDefined, trailingWS = trailingWS)
              }
        }

        // Finally, pick up any tag body contents (if block tag)
        stage4 <- stage3.isBlock match {
          case true if stage3.arity < 4 =>
            P(
              (!closeBlock(stage3.arity, stage3.expr.name) ~/ renderable).rep ~ whitespace.? ~ closeBlock(
                stage3.arity,
                stage3.expr.name) ~ whitespace.?)
              .map {
                case (contents, ws3, closeBlock, ws4) =>
                  // Finalize any tags that may be inside block
                  val finalContents = contents.flatten.foldLeft(Seq.empty[Renderable]) {
                    case (acc, item) =>
                      item match {
                        case tb: TagBuilder => acc ++ tb.finalize
                        case one            => acc :+ one
                      }
                  }
                  // rearrange body and whitespce (and wsCtl) for block (sew te block together to make it look/behave like a single, non-block tag)
                  //                val wsCtlSave = closeBlock.wsCtlAfter
                  val body = Block(
                    OpenTag(stage3.expr, stage3.wsCtlBefore, stage3.wsCtlAfter, stage3.arity),
                    stage3.trailingWS
                      .map(
                        inner2WS =>
                          inner2WS +: ws3
                            .map(inner2WS => finalContents :+ inner2WS)
                            .getOrElse(finalContents))
                      .getOrElse(finalContents),
                    closeBlock
                  )
                  stage3.copy(body       = body, trailingWS = ws4)
              }
          case true if stage3.arity == 4 => // raw block
            P(
              (!closeBlock(stage3.arity, stage3.expr.name) ~ AnyChar).rep.! ~ closeBlock(
                stage3.arity,
                stage3.expr.name) ~ whitespace.?)
              .map {
                case (raw, closeBlock, trailingWS) =>
                  val body = Block(
                    OpenTag(stage3.expr, stage3.wsCtlBefore, stage3.wsCtlAfter, stage3.arity),
                    Seq(Text(raw)),
                    closeBlock
                  )
                  stage3.copy(body       = body, trailingWS = trailingWS)
              }
          case false => P("").map(_ => stage3) // pass thru... non-block tag
        }
      } yield (stage4 match {
        case t if t.ctl.contains("&") =>
          t.copy(arity = 3) // Replace & (no escape) with equivalent arity-3
        case _ => stage4
      }).finalize
    )

  private def closeBlock[_: P](arity: Int, label: String) =
    P(
      "{".rep(arity) ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ "/" ~ spacesOrNL ~ label ~ spacesOrNL ~ wsctl.? ~ spacesOrNL ~ "}"
        .rep(arity))
      .map { case (wcb, wca) => CloseTag(wcb.isDefined, wca.isDefined, arity) }

  //-----------------------------<< Primitives

  private def wsctl[_: P] = P("~").!
  private def ctl[_: P] = P("#*" | "#>" | CharIn("!^#>/&")).!

  private def spaces[_: P] = P(CharsWhileIn(" \t", 0))
  private def spacesOrNL[_: P] = P(CharsWhileIn(" \t\n", 0))
  private def leadingSpace[_: P] = P(CharsWhileIn(" \t\n", 0)).!
  private def trailingSpace[_: P] = P(spaces ~ "\n".?).!

  private def expr[_: P] = P(subExpr | simpleExpr)
  private def subExpr[_: P] = P(exprArg ~ arg.rep).map {
    case (e, a) => new ParsedExpression(e, a.toList)
  }
  private def simpleExpr[_: P] = P(unparsedPath ~ spacesOrNL ~ (arg ~ spacesOrNL).rep).map {
    case (up, a) => ParsedExpression(up, a.toList)
  }
  private def asClause[_: P] =
    P("as" ~ spacesOrNL ~ "|" ~ spacesOrNL ~ (label ~ spacesOrNL).rep ~ "|")

  private def arg[_: P]: P[Argument] = P(assignmentArg | stringArg | literal | pathArg | exprArg)
  private def literal[_: P] =
    P("true" | "false" | "null" | "undefined" | number).!.map(r => LiteralArgument(r))
  private def number[_: P] = P(("-".? ~~ CharsWhileIn("0-9") ~~ ("." ~~ CharsWhileIn("0-9")).?).!)
  private def stringArg[_: P] =
    P("\"" ~~ CharsWhile(_ != '"').! ~~ "\"" | "'" ~~ CharsWhile(_ != '\'').! ~~ "'").map(r =>
      StringArgument(r))
  private def pathArg[_: P] = P(!"as" ~ unparsedPath).map(p => PathArgument(p))
  private def exprArg[_: P] =
    P("(" ~ spacesOrNL ~ simpleExpr ~ spacesOrNL ~ ")")
      .map(e =>
        ExpressionArgument(TagBuilder(3, expr       = e, trailingWS = None).finalize.head
          .asInstanceOf[HelperTag]))

  private def literalArg[_: P] =
    P(
      "true" | "false" | "null" | "undefined" | "-".? ~~ CharsWhile(_.isDigit) ~~ ("." ~~ CharsWhile(
        _.isDigit)).?).!.map(r => StringArgument(r))
  private def assignmentArg[_: P] =
    P(label ~ "=" ~ P(literalArg | pathArg | stringArg)).map(r => AssignmentArgument(r._1, r._2))
  private def label[_: P]: P[String] = CharsWhileIn("""_a-zA-Z0-9""").repX(1).!
  private def unparsedPath[_: P] = P("@partial-block".! | _unparsedPath)
  private def _unparsedPath[_: P] =
    P("@".?.! ~~ CharsWhileIn("""_a-zA-Z0-9./[]""").repX(1).!).map(s => s._1 + s._2)

  // Grab next text string, but separating any trailing whitespace, until either end-of-input or '{{'
  // Returns either Seq(Text), or Seq(Text,Whitespace)
  def text(implicit ctx: P[_]): P[Seq[Renderable]] = {
    val start = ctx.index

    if (start == ctx.input.length)
      ctx.freshFailure()
    else {
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
        while (i < ctx.input.length && ctx.input(i) != '{') i += 1
        if (i == ctx.input.length) // end of input
          None
        else
          Some(i)
      }

      findDoubleBrace(start) match {
        case Some(p) =>
          // Now backtrack over any whitespace
          var k = p - 1
          while (k >= 0 && k >= start && ctx.input(k).isWhitespace) k -= 1
          if (k < 0)
            ctx.freshFailure()
          else {
            ctx.freshSuccess(
              Seq(
                Text(ctx.input.slice(start, Math.max(0, k) + 1)),
                Whitespace(ctx.input.slice(k + 1, p))),
              p)
          }
        case None =>
          ctx.freshSuccess(Seq(Text(ctx.input.slice(start, ctx.input.length))), ctx.input.length) // take rest of string...no "{{" found
      }
    }
  }

  // Grab whitespace until end-of-input or '{{'
  def whitespace(implicit ctx: P[_]): P[Whitespace] = {
    val start = ctx.index
    var i = start
    while (i < ctx.input.length && ctx.input(i).isWhitespace) i += 1
    if (i == start)
      ctx.freshFailure()
    else
      ctx.freshSuccess(Whitespace(ctx.input.slice(start, i)), i)
  }

  //-----------------------------<< Compile!

  def compile(input: String): Seq[Renderable] = {
    parse(input, template(_)) match {
      case Parsed.Success(value, _) => PostParse.clean(value)
      case f @ Parsed.Failure(label, index, extra) =>
        throw new BarsException("Template parsing failed: " + f.toString)
    }
  }
}
