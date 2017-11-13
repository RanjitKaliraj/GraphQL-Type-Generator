import java.util.Date

import com.github.raka.gqltypegen.{AllowedNonScalarInputType, CustomScalarType}
import com.github.raka.gqltypegen.GqlTypeGenerator

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

/**
  * Created by Ranjit Kaliraj on 11/9/17.
  */
object Main extends App{

  case class Foo(a: String, b: List[Bar], c: Option[Bar], lst: List[Int], date: Option[List[Date]])
  case class Bar(name: String, address: String, phone: Option[List[String]], barType: Enum.Value)

  object Enum extends Enumeration {
    val FOO, BAR = Value
  }

  //Instantiate Generator.
  val generator = GqlTypeGenerator.getInstance

  //########### Note: Generating Mutation and QUERY type at same time is not thread safe.

  //Query Type Generation.
  //#########################################################################

  //DateTime is not default scalar type, need to define custom type for this.
  // We will convert it into StringType scalar type of graphql.
  val dateTimeCustomTypedef = new CustomScalarType[Date, String] {
    override def typeConversionCode(input: Date): universe.Tree = reify{
      input.toString()
    }.tree
  }
  //Add the custom defined type.
  generator.addCustomScalarTypeDef(dateTimeCustomTypedef)

  //Generate types for Class Apple.
  //Types will be generated in root directory of your project or this method return generated scala file.
  generator.generateQueryTypes[Foo]


  //Mutation Type Generation.
  //#########################################################################

  //Allowing DateTime as scalartype as StringType.
  //Note - Sangria graphql must support implicit conversion between these types.
  generator.allowNonScalarType(new AllowedNonScalarInputType[Date, String])

  //Finally generating types.
  generator.generateMutationTypes[Foo]
}
