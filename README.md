# GraphQL-Type-Generator
A Scala Case Class to Sangria-GraphQL Type Converter.

## Getting Started
1. Clone the Project:
`git clone git@github.com:RanjitKaliraj/GraphQL-Type-Generator.git`
2. Build:
`sbt publishLocal` 

Import library in your project.
*Note: You can build with your project-matching scala version. Current Version is 2.11.8.*
`libraryDependencies += "com.github.raka" % "gql-type-generator" % "1.0"`

# Usage:
Converting the following Foo into Sangria-GraphQL type.
```
case class Foo(a: String, b: List[Bar], c: Option[Bar], lst: List[Int], date: Option[List[Date]])
case class Bar(name: String, address: String, phone: Option[List[String]], barType: Enum.Value)

object Enum extends Enumeration {
  val FOO, BAR = Value
}
```
Import required files:
`import com.github.raka.gqltypegen._`

Instantiate Generator.
`val generator = GqlTypeGenerator.getInstance`

Define custom type for type that is unknown to GraphQL along with conversion code.
```
val dateTimeCustomTypedef = new CustomScalarType[Date, String] {
  override def typeConversionCode(input: Date): universe.Tree = reify{
    input.toString()
  }.tree
}
//Add the custom defined type.
generator.addCustomScalarTypeDef(dateTimeCustomTypedef)
```
In addition, you can also map a type to graphQL Scalar types with out conversion codes.
`generator.allowNonScalarType(new AllowedNonScalarInputType[Date, String])`

Finally generate type.
```
generator.generateQueryTypes[Foo]
generator.generateMutationTypes[Foo]
```

## Output:
### Query Types
```
import sangria.schema._
import Main.Foo
import java.util.Date
import Main.Bar
import Main.Enum

//Auto Generated GraphQL Types Definition for Entity Foo.
//Generated At: Mon Nov 13 13:56:40 NPT 2017

object FooQueryTypeDefinition{

	lazy val EnumEnumType = EnumType(
		"Enum_EnumType",
		Some("Enum Enumeration Type Definition."),
		List(
			EnumValue("BAR", value = Enum.BAR, description = Some("Enum value BAR for Enum class Enum")),
			EnumValue("FOO", value = Enum.FOO, description = Some("Enum value FOO for Enum class Enum"))
		)
	)
	lazy val BarObjectType = ObjectType(
		"Bar_ObjectType",
		"Bar GraphQL Type Definition for Query",
		fields[Unit, Bar](
			Field("barType", EnumEnumType, resolve = _.value.barType),
			Field("phone", OptionType(ListType(StringType)), resolve = _.value.phone),
			Field("address", StringType, resolve = _.value.address),
			Field("name", StringType, resolve = _.value.name)
		)
	)
	lazy val FooObjectType = ObjectType(
		"Foo_ObjectType",
		"Foo GraphQL Type Definition for Query",
		fields[Unit, Foo](
			Field("date", OptionType(ListType(StringType)), resolve = { v => {
				def resolve(input: java.util.Date): String = input.toString()
				v.value.date match {
					case Some(p) => {
						val res = p.map(q => {
							resolve(q)
						})
						Some(res)
					}
					case None => None
				}
			}}),
			Field("lst", ListType(IntType), resolve = _.value.lst),
			Field("c", OptionType(BarObjectType), resolve = _.value.c),
			Field("b", ListType(BarObjectType), resolve = _.value.b),
			Field("a", StringType, resolve = _.value.a)
		)
	)
}
```

### Mutation Types
```import sangria.schema._
import Main.Foo
import java.util.Date
import Main.Bar
import Main.Enum

//Auto Generated GraphQL Types Definition for Entity Foo.
//Generated At: Mon Nov 13 13:56:40 NPT 2017

object FooMutationTypeDefinition{

	lazy val EnumEnumType = EnumType(
		"Enum_EnumType",
		Some("Enum Enumeration Type Definition."),
		List(
			EnumValue("BAR", value = Enum.BAR, description = Some("Enum value BAR for Enum class Enum")),
			EnumValue("FOO", value = Enum.FOO, description = Some("Enum value FOO for Enum class Enum"))
		)
	)
	lazy val BarInputObjectType = InputObjectType(
		"Bar_InputObjectType",
		"Bar GraphQL Input Type Definition for Mutation",
		List(
			InputField("barType", EnumEnumType, description = "barType Enum Value."),
			InputField("phone", OptionType(ListType(StringType)), description = "phoneof type String"),
			InputField("address", StringType, description = "addressof type String"),
			InputField("name", StringType, description = "nameof type String")
		)
	)
	lazy val FooInputObjectType = InputObjectType(
		"Foo_InputObjectType",
		"Foo GraphQL Input Type Definition for Mutation",
		List(
			InputField("date", OptionType(ListType(StringType)), description = "dateof type BigInt"),
			InputField("lst", ListType(IntType), description = "lstof type Integer"),
			InputField("c", OptionType(BarInputObjectType), description = "c input field"),
			InputField("b", ListType(BarInputObjectType), description = "b input field"),
			InputField("a", StringType, description = "aof type String")
		)
	)
}
```
