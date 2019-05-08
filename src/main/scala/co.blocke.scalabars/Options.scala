package co.blocke.scalabars

case class Options(
    handlebars:  Scalabars,
    context:     Option[Context]  = None, // i.e. the stack, the global context, etc.
    helperName:  String           = "",
    blockParams: List[String]     = List.empty[String],
    params:      List[Argument]   = List.empty[Argument],
    fn:          Option[Template] = None,
    inverse:     Option[Template] = None,
    hash:        Map[String, Any] = Map.empty[String, Any]
)

/*
 Runtime:

    "this" -- ??? maybe context + assignments?
    hash -- ??? maybe compile options?
    params -- passed in
    blockParams -- custom stuff for blocks (stock Handlebars)--not MVP
 */ 