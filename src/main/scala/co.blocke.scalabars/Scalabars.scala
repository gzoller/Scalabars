package co.blocke.scalabars

import model._
import renderables._
import parsing._
import org.graalvm.polyglot.{ Context => JSContext }

import helpers.collections._
import helpers.stock._
import helpers.comparison._
import helpers.misc._

object Scalabars {
  def apply(logger: Option[SBLogger] = None): Scalabars = Scalabars(
    Map(
      // Stock Handlebars
      "each" -> EachHelper(),
      "if" -> IfHelper(),
      "lookup" -> LookupHelper(),
      "unless" -> UnlessHelper(),
      "with" -> WithHelper(),
      "helperMissing" -> HelperMissingHelper(),
      "blockHelperMissing" -> BlockHelperMissingHelper(),
      "log" -> LogHelper(),
      // Comparisons
      "eq" -> EqHelper(),
      "ne" -> NeHelper(),
      "or" -> OrHelper(),
      "and" -> AndHelper(),
      // Collections
      "first" -> FirstHelper(),
      "last" -> LastHelper(),
      "empty" -> EmptyHelper(),
      "withTake" -> WithTakeHelper(),
      "withDrop" -> WithDropHelper(),
      "withFirst" -> WithFirstHelper(),
      "withLast" -> WithLastHelper(),
      "contains" -> ContainsHelper(),
      "any" -> AnyHelper(),
      "join" -> JoinHelper(),
      "length" -> LengthHelper(),
      "lengthEquals" -> LengthEqualsHelper(),
      "sortEach" -> SortEachHelper(),
      "withLookup" -> WithLookupHelper(),
      // Misc
      "default" -> DefaultHelper(),
      "include" -> IncludeHelper(),
      "markdown" -> MarkdownHelper(),
      "raw" -> RawHelper(),
      "url" -> UrlHelper(),
      // Internal
      "else" -> ElseHelper()
    ),
    collection.mutable.Map.empty[String, PartialHelper],
    logger.getOrElse(JavaLogger()),
    NoopFileGetter()
  )
}

case class Scalabars(
    private val helpers:             Map[String, Helper],
    private[scalabars] val partials: collection.mutable.Map[String, PartialHelper],
    logger:                          SBLogger,
    fileGetter:                      FileGetter
) {

  override def toString: String = "Scalabars(helpers=" + helpers.keys.mkString("[", ",", "]") + ")"

  private lazy val parser = HandlebarsParser()(this)

  private lazy val stockOptions: Map[String, EvalResult[_]] = Map(
    "noEscape" -> BooleanEvalResult(false),
    "strict" -> BooleanEvalResult(false),
    "preventIndent" -> BooleanEvalResult(false),
    "explicitPartialContext" -> BooleanEvalResult(false),
    "knownHelpersOnly" -> BooleanEvalResult(false)
  )

  // This is here so we get a new Context (javascript engine) for every Scalabars() instance.
  private[scalabars] lazy val js = {
    val ctx = JSContext.newBuilder().allowAllAccess(true).build()
    val bindings = ctx.getBindings("js")
    bindings.putMember("Handlebars", Handlebars(this))
    ctx
  }

  def registerHelper(name: String, helperJS: String): Scalabars =
    this.copy(helpers = helpers + (name -> JSHelper(name, helperJS)))
  def registerHelper(name: String, helper: Helper): Scalabars =
    this.copy(helpers = helpers + (name -> helper))
  def registerPartial(name: String, script: String): Scalabars = {
    val template = compile(script)
    partials.put(
      name,
      PartialHelper(
        name,
        template.copy(compiled = OpenTag(ParsedExpression(name), false, false, 3) +: template.compiled :+ CloseTag(false, false, 3))))
    this
  }

  def setFileGetter(fileGetter: FileGetter) = this.copy(fileGetter = fileGetter)

  def compile(rawTemplate: String, compileOptions: Map[String, Boolean] = Map.empty[String, Boolean]) = {
    val hashArgs = stockOptions ++ compileOptions.map { case (k, v) => (k, BooleanEvalResult(v)) }
    SBTemplate(parser.compile(rawTemplate)(compileOptions).toList, Options(this, _hash = hashArgs))
  }

  private[scalabars] def getHelper(name: String): Option[Helper] = helpers.get(name)
  private[scalabars] def getPartial(name: String) = partials.get(name)
}

/*
data: Set to false to disable @data tracking.
compat: Set to true to enable recursive field lookup.
knownHelpers: Hash containing list of helpers that are known to exist (truthy) at template execution time. Passing this allows the compiler to optimize a number of cases. Builtin helpers are automatically included in this list and may be omitted by setting that value to false.
knownHelpersOnly: Set to true to allow further optimzations based on the known helpers list.
noEscape: Set to true to not HTML escape any content.
strict: Run in strict mode. In this mode, templates will throw rather than silently ignore missing fields. This has the side effect of disabling inverse operations such as {{^foo}}{{/foo}} unless fields are explicitly included in the source object.
assumeObjects: Removes object existence checks when traversing paths. This is a subset of strict mode that generates optimized templates when the data inputs are known to be safe.
preventIndent: By default, an indented partial-call causes the output of the whole partial being indented by the same amount. This can lead to unexpected behavior when the partial writes pre-tags. Setting this option to true will disable the auto-indent feature.
ignoreStandalone: Disables standalone tag removal when set to true. When set, blocks and partials that are on their own line will not remove the whitespace on that line.
explicitPartialContext: Disables implicit context for partials. When enabled, partials that are not passed a context value will execute against an empty object.
 */

// jdk.nashorn.api.scripting.ScriptObjectMirror.call(thiz, obj...)
