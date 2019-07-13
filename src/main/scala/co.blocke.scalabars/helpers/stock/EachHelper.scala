package co.blocke.scalabars
package helpers.stock

import model._
import org.json4s._

case class EachHelper(iterateObjects: Boolean = true) extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a) if a.arr.nonEmpty =>
        a.arr.indices.map { i =>
          val index = LongEvalResult(i.toLong)
          val first = BooleanEvalResult(i == 0)
          val last = BooleanEvalResult(i == a.arr.size - 1)
          val iterationCtx = (arg("items") >> LongEvalResult(i)).get.flatten()
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
        }.mkString
      case AsObject(o) if o.children.nonEmpty && iterateObjects =>
        o.values.keySet.map { key =>
          val iterationCtx = (arg("items") >> StringEvalResult(key)).get.flatten()
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
        }.mkString
      case _ =>
        options.inverse()
    }
}
