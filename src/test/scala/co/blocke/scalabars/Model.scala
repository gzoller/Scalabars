package co.blocke.scalabars

case class Person(name: String, age: Int)
case class Desc(heavy: String)
case class Data(
    name:   String,
    msg:    String,
    aNum:   Int,
    isOK:   Boolean,
    small:  Long,
    A:      List[Desc],
    player: Person
)
case class Magic(name: String, stuff: Map[String, Int])

case class Stuff(
    foo: List[String],
    bar: Map[String, String]
)
case class Stuff2(
    foo:   Map[String, String],
    bar:   Map[String, Int],
    thing: String
)
