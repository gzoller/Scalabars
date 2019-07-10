  
# JavaScript Helpers  
Scalabars supports Javascript native helpers, just like stock Handlebars.  We can't claim 100% no-fuss compatibility, but it does do a pretty good job of being close.

You register JavaScript helpers like this:

```scala
val sb = Scalabars().registerHelper(  
  "jsEach",  
  """  
   |function(arr, options) { |  if(!arr || arr.length === 0) { 
   |    return options.inverse(); 
   |  } 
   | 
   |  var data={}; 
   |  if( options.data ) { 
   |    data = Handlebars.createFrame(options.data); 
   |  } 
   | 
   |  var result = []; 
   |  for(var i=0; i<arr.length; i++) { 
   |    if(data) { 
   |      data.index = i; 
   |    } 
   |    result.push(options.fn(arr[i], {data: data})); 
   |  } 
   | 
   |  return result.join(''); 
   |}""".stripMargin)
```
After being registered, you can use your Javascript helper just like a Scala helper in your templates.

Note that the Handlebars facilities for 'options' and 'data' are present and functional.

Your best bet in Scalabars is to write your helpers in Scala--that's why this system exists!  But, if you have that favorite helper in Javascript, give it a try in Scalabars and see if it works.

