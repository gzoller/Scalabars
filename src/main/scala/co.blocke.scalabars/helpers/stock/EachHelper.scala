package co.blocke.scalabars
package helpers.stock

import model._
import org.json4s._

case class EachHelper(iterateObjects: Boolean = true) extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    arg("items") match {
      case AsArray(a) if a.arr.nonEmpty =>
        a.arr.indices.map { i =>
          val index = LongEvalResult(i.toLong)
          val first = BooleanEvalResult(i == 0)
          val last = BooleanEvalResult(i == a.arr.size - 1)
          arg("items") >> LongEvalResult(i) match {
            case Some(ctx) =>
              val iterationCtx = ctx.flatten()
              val addedBlockParams =
                options.blockParams.zipWithIndex.foldLeft(Map.empty[String, Context]) {
                  case (bpMap, pair) =>
                    pair match {
                      case (bp, 0) => addBlockParam(bpMap, bp, iterationCtx)
                      case (bp, 1) =>
                        addBlockParam(bpMap, bp, iterationCtx.push(JInt(index.value), "$index"))
                      case (bp, 2) =>
                        addBlockParam(bpMap, bp, iterationCtx.push(JBool(first.value), "$first"))
                      case (bp, 3) =>
                        addBlockParam(bpMap, bp, iterationCtx.push(JBool(last.value), "$last"))
                      case _ => bpMap // do nothing
                    }
                }
              val data = Map("index" -> index, "first" -> first, "last" -> last)
              options.fn(iterationCtx, data, addedBlockParams)
            case _ => options.inverse()
          }
        }.mkString
      case AsObject(o) if o.children.nonEmpty && iterateObjects =>
        o.values.keySet.map { key =>
          arg("items") >> StringEvalResult(key) match {
            case Some(ctx) =>
              val iterationCtx = ctx.flatten()
              val addedBlockParams =
                options.blockParams.zipWithIndex.foldLeft(Map.empty[String, Context]) {
                  case (bpMap, pair) =>
                    pair match {
                      case (bp, 0) => addBlockParam(bpMap, bp, iterationCtx)
                      case (bp, 1) =>
                        addBlockParam(bpMap, bp, iterationCtx.push(JString(key), "$key"))
                      case _ => bpMap // do nothing
                    }
                }
              options.fn(iterationCtx, Map("key" -> StringEvalResult(key)), addedBlockParams)
            case _ => options.inverse()
          }
        }.mkString
      case o: ContextEvalResult if o.isObject && o.children.nonEmpty => // not iterating the kids...just return the object
        val bpMap = options.blockParams.headOption
          .map(bp => addBlockParam(Map.empty[String, Context], bp, o.value))
          .getOrElse(Map.empty[String, Context])
        options.fn(o.value, Map.empty[String, EvalResult[_]], bpMap)
      case _ =>
        options.inverse()
    }
  }
}
