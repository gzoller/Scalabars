package co.blocke.scalabars

import SB._
import org.json4s._
import jdk.nashorn.api.scripting.JSObject
import scala.util.Try

case class BlockRunner(context: Context, options: Map[String, Any], contents: List[Renderable], elseBlock: List[Renderable])(implicit sb: Scalabars) {
  def fn(a: JSObject): String = {
    val newScope = ScriptToJson4s.toJson4s(a)
    renderChunk(Context(newScope, newScope +: context.history.tail), options, contents)
  }
  def inverse(a: JSObject): String = {
    val newScope = ScriptToJson4s.toJson4s(a)
    renderChunk(Context(newScope, newScope +: context.history.tail), options, elseBlock)
  }
}

case class Block(expr: Expr, contents: List[Renderable]) extends Renderable {
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String = {
    val (fnBlock, inverseBlock) = examineBlock(contents)
    expr match {
      // =======
      // Native helper support (pure Scala vs Javascript implementation for efficiency
      // =======
      case f: FullExpr if f.label == "each" =>
        val arr = context.find(f.args.head.asInstanceOf[PathArgument].path, options).value.asInstanceOf[JArray].arr
        if (arr.isEmpty)
          renderChunk(context, options, inverseBlock)
        else
          arr.map(a =>
            renderChunk(Context(a, a +: context.history.tail), options, fnBlock)
          ).mkString
      case f: FullExpr if f.label == "with" =>
        val pre = context.find(f.args.head.asInstanceOf[PathArgument].path, options).value
        if (pre == JNothing || (pre.isInstanceOf[JObject] && pre.asInstanceOf[JObject].children.size == 0))
          renderChunk(context, options, inverseBlock)
        else {
          val a = pre.asInstanceOf[JObject]
          renderChunk(Context(a, a +: context.history.tail), options, contents)
        }
      case f: FullExpr if f.label == "if" =>
        context.find(f.args.head.asInstanceOf[PathArgument].path, options).value match {
          case JNothing            => renderChunk(context, options, inverseBlock)
          case JNull               => renderChunk(context, options, inverseBlock)
          case b: JBool if b.value => renderChunk(context, options, fnBlock)
          case _: JBool            => renderChunk(context, options, inverseBlock)
          case _                   => renderChunk(context, options, fnBlock)
        }
      case f: FullExpr if f.label == "unless" =>
        context.find(f.args.head.asInstanceOf[PathArgument].path, options).value match {
          case JNothing            => renderChunk(context, options, fnBlock)
          case JNull               => renderChunk(context, options, fnBlock)
          case b: JBool if b.value => renderChunk(context, options, inverseBlock)
          case _: JBool            => renderChunk(context, options, fnBlock)
          case _                   => renderChunk(context, options, inverseBlock)
        }
      case f: FullExpr if f.label == "eq" =>
        val op1 = unpackArg(f.args(0), context, options)
        val op2 = unpackArg(f.args(1), context, options)
        if (op1.values == op2.values)
          renderChunk(context, options, fnBlock)
        else
          renderChunk(context, options, inverseBlock)
      case f: FullExpr if f.label == "ne" =>
        val op1 = unpackArg(f.args(0), context, options)
        val op2 = unpackArg(f.args(1), context, options)
        if (op1.values != op2.values)
          renderChunk(context, options, fnBlock)
        else
          renderChunk(context, options, inverseBlock)
      case f: FullExpr if f.label == "or" =>
        val ops = f.args.map(a => unpackArg(a, context, options))
        val cond = ops.foldLeft(false) {
          case (a, b) => a | (b match {
            case b: JBool => b.value
            case JNothing => false
            case JNull    => false
            case _        => true
          })
        }
        if (cond)
          renderChunk(context, options, fnBlock)
        else
          renderChunk(context, options, inverseBlock)
      case f: FullExpr if f.label == "and" =>
        val ops = f.args.map(a => unpackArg(a, context, options))
        val cond = ops.foldLeft(true) {
          case (a, b) => a & (b match {
            case b: JBool => b.value
            case JNothing => false
            case JNull    => false
            case _        => true
          })
        }
        if (cond)
          renderChunk(context, options, fnBlock)
        else
          renderChunk(context, options, inverseBlock)

      // =======
      // Javascript/User-Provided Helpers
      // =======
      case s: SimpleExpr if sb.helpers.contains(s.label) =>
        sb.javascript.put("__options", BlockRunner(context, options, fnBlock, inverseBlock)) // gymnastics to pass an object to options.fn()
        sb.run(FullExpr(s.label, List(ObjectArgument("__options"))), context, options)
      case s: SimpleExpr =>
        context.find(s.path, options).value match {
          case JNothing             => ""
          case b: JBool if !b.value => ""
          case _                    => renderChunk(context, options, fnBlock)
        }
      case f: FullExpr =>
        sb.javascript.put("__options", BlockRunner(context, options, fnBlock, inverseBlock)) // gymnastics to pass an object to options.fn()
        sb.run(FullExpr(f.label, f.args :+ ObjectArgument("__options")), context, options)
    }
  }

  private def unpackArg(a: Argument, context: Context, options: Map[String, Any]) = a match {
    case p: PathArgument => p match {
      case a if a.path == List("true")      => JBool(true)
      case a if a.path == List("false")     => JBool(false)
      case a if a.path == List("null")      => JNull
      case a if a.path == List("undefined") => JNothing
      case a if a.path.length == 1 && a.path.head.isNumeric() =>
        Try(p.path.head.toLong).toOption.map(m => JLong(m)).getOrElse(JDouble(p.path.head.toDouble))
      case _ => context.find(p.path, options).value
    }
    case s: StringArgument => JString(s.value)
  }

  private def examineBlock(c: List[Renderable]): (List[Renderable], List[Renderable]) = {
    val elseLoc = c.indexWhere(a => a.isInstanceOf[Thing] && a.asInstanceOf[Thing].expr.label == "else")
    if (elseLoc >= 0) {
      val (a, b) = c.splitAt(elseLoc)
      (a, b.tail)
    } else
      (c, List.empty[Renderable])
  }
}