
# Scala Helpers
Scalabars custom helpers can be written in Scala and registered with the Scalabars system like this:
```scala
    val sb = Scalabars().registerHelper("myHelper", MyHelper())
```
Once registered you can use them as you would any native helper:
```scala
    {{# myHelper}}Interesting stuff here{{/myHelper}}
```
Note that your helpers can either be block (as shown above) or non-block.  It's important to choose one of these modes before writing your helper.

## Non-Block Helper
A non-block helper is the easiest to write and use.  Here's a sample:
```scala
case class Sample() extends Helper("x","y") {  
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =  
    s"""Greetings ${arg("x")}!  Do you like ${arg("y")}?"""  
}
```
Used like this:

```scala
sb.registerHelper("sample",Sample())
sb.compile("""{{sample "Mike" "ice cream"}}""")(json)
```
Here we passed in 2 string-literal arguments.  Helpers have a number of handy functions available to do useful things.  arg() is one that resolves an argument symbol ('x' or 'y' in our example) to a resolved value.  A list of helper functions is provided below.

Note that a helper's run() function returns an EvalResult, however there are implicit conversions for several scalar types, String for example, to make life easier.

When writing a non-block helper, the critical thing to know is that you must return a scalar value.  If you return an Array or Object, the rendered output will not be what you expect.  For example an object would render "[object Object]", which isn't terribly useful.

You'll likely ignore the partials implicit parameter to run().  It's there mainly to be (implicitly) passed to other functions in the machinery.  It contains context-aware partial definitions.  (For example if you define an inline partial "foo" in the current block, vs a "foo" partial in a parent context.)  Best not to mess with it.

The options parameter is far more useful.  It holds a reference to the top-level Scalabars object, as well as a Context object (used for looking things up), block parameters (discussed in a bit), access to your hash and data maps, and fn() function used for rendering blocks. 

## Block Helper
Block helpers are almost the same as non-block helpers but with one key difference.  They don't return a scalar value.  Instead, when you want to "return" from a block helper you need to call one of the fn() flavors (see Options notes below) or inverse().  These will render either the main body of the block, or in the case of inverse() the block's 'else' clause if one exists.

Calling fn() is typically the very last thing you'd do in a block helper, but sometimes (for example if your helper iterates) you call fn() for each iteration and combine the Strings with mkstring (see EachHelper).  That's only when you're rendering the block multiple times.

## Helper Functions
These are some handy functions automatically exposed for use in your Helper's run() function:

#### arg(symbol: String): EvalResult[_] 
Given an argument symbol, resolve it to its base value, scalar or context (array or object).  EvalResult is essentially a wrapper for either scalar or Context.

#### scalarArg(symbol: String): Option[Any]
Like arg() but when we know (or wish to assert) that the argument is a scalar value

#### contextArg(symbol: String):  Option[Context]
Like arg() but when we know (or wish to assert) that the argument is a Context (array or object) value

#### rawArgValue(symbol: String): String
This is typically used for error messages.  It returns the textual value of the argument (the value the caller passed in).

#### lookup(basePath: String, incrementalPath: String): Context
This is a way to look up a Context given a constructed path.  An example might help make sense of this:
```scala
case class WithDropHelper() extends Helper("items", "loc") {  
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =  
    Try(scalarArg("loc").get.asInstanceOf[Long].toInt) match {  
      case Success(loc) =>  
        contextArg("items") match {  
          case Some(c) =>  
            c.value match {  
              case a: JArray =>  
                a.arr.drop(loc).indices.map(i => options.fn(lookup(c.path, s"[${i + loc}]"))).mkString  
              case _ => options.inverse()  
            }  
          case None =>  
            options.inverse()  
        }  
      case _ => options.inverse()  
    }  
}
```
This helper expects arguments of an Array (items) and an integer (loc), and will drop the first loc values from items and use the result as the collection to iterate over the block body's contents.  The key for our example is this part:  `lookup(c.path, s"[${i + loc}]")`.

Here we have already successfully resolved items into a Context (array) and assigned it to c.  Contexts have a path from root '/'.  So this lookup starts from c's path (i.e. the full path to the array passed in) and then extends a calculated relative path of [i+loc].

If you don't need fancy relative paths, you could always just do this:

```scala
lookup(".", "my/path.[0]")
lookup("/", "my/path.[0]")
```

## Options Object
The Options object is passed around implicitly almost everywhere and is available in your Helper's run() function.  Here's a quick rundown of useful data and functions in Options.

### Data
```scala
handlebars:  Scalabars,            // top-level Scalabars reference 
context:     Context               // Context object  
helperName:  String                // name of this helper (in case you forgot)
blockParams: List[String]          // list of block parameters given by caller 
params:      List[EvalResult[_]]   // parameter symbols
paramValues: List[String]          // textual values of parameter symbols
```
### Functions
#### fn(): String
Render the primary (non-'else') contents of your helper's block body.
#### fn(c: Context): String
As above, but given a Context.  This is handy if you want to force the context of the render, for example if iterating through a list of objects you'd call fn() and pass a context for each item in your iteration.  (see EachHelper code)
#### fn(c: Context, data: Map[String, EvalResult[_]]): String
As above but also passing a data Map (see EachHelper code)
#### fn(c: Context, data: Map[String, EvalResult[_]], blockParams: Map[String, Context]): String
As above bus also passing block parameters (see EachHelper code)
#### inverse(): String
Render the inverse ('else') block, or "" if none defined.
#### hash(key: String): String
Return a value set in the hash (assignment argument).  Returns "" if key value is unknown.
#### isFalsy(er: EvalResult[_]): Boolean
Since Handlebars is originally a JavaScript creature, data types are, um, squishy.  This function does its best to determine "false" boolean value from a number of sources, for example boolean false, String "false", JBool false, JNothing, empty array/object, etc.)  You can use this with the return value from arg() to test the "falseness" of your argument if that's useful.


## Hash vs Data
Handlebars supports two ways of holding data in your helpers, the hash and a data map.  The has is populated with assignment arguments by the caller:

```
{{myHelper x=1, y="foo"}}
```
In this example you can access x and y in your helper with:
```scala
options.hash("x") // "1" (hash() always returns a String)
options.hash("y") // "foo"
```

Data blocks are often used for pushing data out of your helper for use in the content itself using the '@' decoration.

```scala
case class Example() extends Helper() {  
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    // Set a data value we want visible in the content body
    val newData = Map("hey"->StringEvalResult("you"))
    options.fn(options.context, newData)
  }
}
```
Now use it...
```
{{#example}}
  I am {{@hey}}
{{/example}}
```
Prints: "I am you"

## Block Parameter Support
Block parameters are an interesting way of avoiding naming conflicts in nested helpers.  They're a standard Handlebars invention.  Your helper must support them.  Not many of the included Scalabars helpers do--in fact only 'with' and 'each' helpers do.

Block parameter assignments are ordered and optional.

Each exports  4 possible block parameters:
  Position 0: This block iteration's context
  Position 1: iteration index
  Position 2: set true if first in iteration order
  Position 3: set true if lats in iteration order

You can set block parameters to whatever you want--they're purely an ordered list of strings to the engine.

You'd use these like this:

```
{{# each strs as |ctx1 idx1 isFirst1 isLast1|}}  
   outer: {{idx1}} first? {{isFirst1}}  last? {{isLast1}}
   {{# each . as |ct2 idx2 isFirst2 isLast2|}}
      inner: {{idx2}} first? {{isFirst2}}  last? {{isLast2}}
   {{/each}}
{{/each}}
```

This produces output:
```
  outer: 0 first? true  last? false
    inner: 0 first? true  last? false
    inner: 1 first? false  last? false
    inner: 2 first? false  last? true
  outer: 1 first? false  last? true
    inner: 0 first? true  last? false
    inner: 1 first? false  last? true
```
Note that we needed to specify ctx1 and ctx2 even though we didn't use them.  That's because block parameters are ordered, meaning the EachHelper assigns Context as the first position block parameter, whether you need it or not!


## EvalResult '>>' operator
There are many times in your helper when you want  to "sub-index" from a resolved argument value.  Consider this data:

```json
{
"interests": [{  
    "item":"car",  
    "label":"Porsche 356"  
  },{  
    "item":"boat",  
    "label":"FPB 78"  
  }]
}
```
Let's say I pass an argument to this list of objects, and I expect this structure and want to dive deeper in my helper.  Let's say I want to resolve [1].label from the passed list of objects.  That looks like this:

```scala
// If called with {{myHelper interests}} and 'x' receives the 'interests' array
arg("x") >> StringEvalResult("[1].label")
```

So why the fancy operator?  There's a lot of logic needed here because you don't know what type arg("x") resolves to so there's quite a bit of error checking.  If 'x' resolved to an object, the notion of drilling deeper isn't an array index like [1], but some field name.  So the same semantic idea of "drilling deeper" is done differently.  The ">>" operator hides all that complexity and just does the work for you, returning Some() of the value if it worked, and None if the drill-down didn't work for some reason ('x' is wrong data type, or invalid index/field).
