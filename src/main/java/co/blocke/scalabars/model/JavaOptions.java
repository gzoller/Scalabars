package co.blocke.scalabars.model;

import co.blocke.scalabars.Scalabars;
import java.util.*;
import scala.collection.JavaConverters;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyObject;
import co.blocke.scalabars.package$;
import org.json4s.JsonAST.*;


/**
 * Options class repackaged as a Java class because graal.js doesn't like Scala classes at the moment.
 * Java classes work just fine, so this is basically a Java proxy object for Options.
 */
public class JavaOptions {

    private Options scalaOptions;

    public Scalabars handlebars;
    public Context context;
    public String helperName;
    public List<String> blockParams;
    public List<Object> params = Collections.emptyList();  // TODO: Populate this!
    public ProxyObject data;
    public ProxyObject hash;

    public JavaOptions( Options scalaOptions ){
        this.scalaOptions = scalaOptions;
        this.handlebars = scalaOptions.handlebars();
        this.context = scalaOptions.context();
        this.helperName = scalaOptions.helperName();
        this.blockParams = JavaConverters.seqAsJavaList(scalaOptions.blockParams());
        this.hash = package$.MODULE$.maper2graalvalue(scalaOptions._hash(), scalaOptions.handlebars());
        this.data = package$.MODULE$.maper2graalvalue(scalaOptions.data(), scalaOptions.handlebars());
    }

    public String fn() { return scalaOptions.fn(); }
    public String fn(Value v) {
        JValue jv = package$.MODULE$.value2JValue(v);
        Context newCtx = context.push(jv, "$jsValue");
        return scalaOptions.fn(newCtx);
    }
    public String fn(Value v, Value newData) {
        JValue jv = package$.MODULE$.value2JValue(v);
        Context newCtx = context.push(jv, "$jsValue");
        return scalaOptions.fn(newCtx, package$.MODULE$.graalvalue2maper(newData, context) );
    }

    public String inverse() { return scalaOptions.inverse(); }
}
