package co.blocke.scalabars
package model

import org.json4s._
import renderables._

// t is defined only for registerd partials.  Inline partials are EmptyTemplate for t
case class PartialHelper(
    name:           String,
    t:              Template,
    firstPass:      Boolean  = true,
    isPartialBlock: Boolean  = false)
  extends Helper("givenContext") {

  private var parent: Option[Renderable] = None
  def setParent(p: Renderable) = {
    parent = Some(p)
    this
  }

  // PartialHelper is complex.  It uses a 2-pass system.  First pass, resolve the template and "become" a BlockHelper (i.e. replace the parent with
  // a BlockHelper holding the partial template.  Then circle 'round and do it again, this time actually replacing the content.
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    if (firstPass) {
      parent.get match {
        case ht: HelperTag =>
          val template = t match {
            case EmptyTemplate() =>
              // If template is empty it means we presume this is a ref to an inline template (stored in context).  Let's go find it...
              // If not found.... error.  This isn't a block, so there's no default thing to render, so go boom.
              options.context.partials
                .getOrElse(name, throw new BarsException(s"No partial named '${name}' registered"))
            case _ => t
          }
          RenderableEvalResult(
            BlockHelper(
              name,
              this.copy(firstPass = false),
              false,
              ht.expr,
              ht.arity,
              Seq.empty[String],
              Block(template).get))

        case bt: BlockHelper =>
          // NOTE: At this point template 't' may be EmptyTemplate.  This is OK for a BlockHelper.  It will be resolved in 2nd pass
          RenderableEvalResult(
            bt.copy(
              helper = this.copy(
                firstPass      = false,
                isPartialBlock = options.context.partials.get(name).isDefined)))
      }
    } else {
      // 2nd Pass
      // For 2nd pass either:
      //    1) This is a non-block partial (resolved) and the contents of the template are in the BlockHelper's body.
      //    2) This is an non-found partial block helper with default body
      //    3) This is a found partial block helper w/body to stuff into @partial-block
      // For #1 just render the body
      // For #2 and #3 look at the isPartialBlock flag (true if #3, false if #2)
      val partialContextCandidate = arg("givenContext") match {
        case NoEvalResult() => options.context
        case e: EvalResult[_] =>
          implicit val baseCtx = options.context
          val c: Context = e // Behold the implicits...
          c
      }

      // NOTE: In Handlebars, AssignmentArguments are merged with the context for partials, not accessed via options.hash, like they are in normal helpers.
      // Therefore, dump hash contents into context before rendering...
      //
      // Unless.... explicitPartialContext is set to true, in which case *only* AssignmentArguments are visible in the partial.  The current
      // context is otherwise thrown away.
      def getHashContents(): List[(String, JValue)] =
        options._hash.map {
          case (k, v) =>
            val v2: JValue = v
            (k, v2)
        }.toList

      val partialContext =
        if (options.hash("explicitPartialContext") == "true")
          partialContextCandidate.copy(value = new JObject(getHashContents()))
        else
          partialContextCandidate.value match {
            case jo: JObject =>
              partialContextCandidate.copy(value = Merge.merge(jo, new JObject(getHashContents())))
            case _ =>
              partialContextCandidate // not an object context... not much we can do about assignments
          }

      if (isPartialBlock) {
        val ctx = partialContext.setData("partial-block", options.fn(partialContext))
        options.context.partials(name).render(ctx)
      } else
        options.fn(partialContext)
    }
}
