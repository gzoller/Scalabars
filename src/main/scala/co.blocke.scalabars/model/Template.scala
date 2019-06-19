package co.blocke.scalabars
package model

import renderables._
import scala.reflect.runtime.universe.TypeTag
import org.json4s._

trait Template {
  val compiled: List[Renderable]
  val options: Options

  def apply(dataObj: JValue): String = render(Context.root(dataObj).copy(partials = options.context.partials))
  def apply[T](dataObj: T)(implicit tt: TypeTag[T]): String = render(contextFromObj(dataObj))

  def render(context: Context): String = ""

  def contextFromObj[T](obj: T)(implicit tt: TypeTag[T]): Context =
    Context.root(toJson4s(obj)).copy(partials = options.context.partials)

  override def toString: String = {
    val tabbed = compiled.map(_.toString).map(s => s.split("\n").map(t => "     " + t).mkString("\n")).mkString("\n")
    "Template:\n" + tabbed + "\n"
  }
}

case class EmptyTemplate() extends Template {
  val options: Options = null // Never used
  val compiled = List.empty[Renderable]
}

case class SBTemplate(compiled: List[Renderable], options: Options) extends Template {

  override def render(context: Context): String = {
    val startingOpts = options.copy(context = context)

    val z = compiled.foldLeft(RenderControl(startingOpts)) { case (rc, renderable) => renderable.render(rc) }.out.toString

    /*
      case (rc, renderable) => renderable.render(rc)

        // Get the raw output and any new Options
        val (newOpts, str) = renderable.render(rc.opts) // TODO: pass in inner ws/ctl for block internal render!!!

        // Now decide what to do with it...
        renderable match {
          case t: Text        => rc.addText(t.s)
          case ws: Whitespace => rc.addWS(ws.ws)

          case r: BlockTag if r.isBlock => // check isBlock here because HelperTag is technically block but can be non-block!
            val closeTag = r.contents.reverse.head.asInstanceOf[CloseTagProxy]
            val stage0 = rc.reset()
            val stage1 = if (r.aloneOnLine) stage0.clipLeading() else stage0
            val stage2 = if (closeTag.aloneOnLine) stage1.clipTrailing() else stage1
            val stage3 = if (r.wsCtlBefore) stage2.flushLeading() else stage2
            val stage4 = if (r.wsCtlAfter) stage3.flushTrailing() else stage3
            stage4.addContent(str).copy(opts = newOpts) // add options just in case this is inline partial, which changes options

          case ot: OpenTagProxy =>
            val stage1 = if (ot.aloneOnLine) rc.clipTrailing() else rc
            val stage2 = if (ot.wsCtlAfter) rc.flushTrailing() else stage1
            println("OPEN: " + ot + " --> " + stage2)
            stage2

          case ct: CloseTagProxy =>
            val stage1 = if (ct.aloneOnLine) rc.clipLeading() else rc
            val stage2 = if (ct.wsCtlAfter) rc.flushLeading() else stage1
            stage2

          case r: Renderable =>
            val stage0 = rc.reset()
            val stage1 = if (r.wsCtlBefore) stage0.flushLeading() else stage0
            val stage2 = if (r.wsCtlAfter) stage1.flushTrailing() else stage1
            stage2.addContent(str)
        }
    }.out.toString
         */

    //    println("-----------------------")
    //    println(z)
    //    println("-----------------------")

    z
  }
}