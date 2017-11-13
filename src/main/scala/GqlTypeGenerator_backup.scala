/*

import java.io.InvalidClassException
import java.util.Calendar
import java.io._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror, universe}
import scala.collection.mutable.ListBuffer

/**
  * A Scala Case Class to GraphQL Type Converter.
  * Note: Type generator will work only for case class model object.
  *
  * Created by Ranjit Kaliraj on 11/6/17.
  */

class com.rew3.gqltypegen.GqlTypeGenerator {

  //Hold custom user defined scalar types.
  private[this] val customScalarTypes = ListBuffer[com.github.raka.gqltypegen.CustomScalarType[_<:Any, _<:Any]]()

  //private[this] val hlist: HList = HNil

  //Holds package name of all found classeses/case classes for import (except primitive.)
  private[this] val imports = new StringBuilder
  imports.append("import sangria.schema._\n")

  def addCustomScalarTypeDef[A <: Any,B <: Any](cType: com.github.raka.gqltypegen.CustomScalarType[A,B]): Unit = {
    customScalarTypes.+=(cType)
    //customScalarTypes = cType :: customScalarTypes
  }

  //Holds all generated GQL types.
  private[this] val generatedGQLTypes = ListBuffer[GQLType]()

  //Generate graphql type.
  def generate[T <: AnyRef : TypeTag]: File = {

    if(Utilities.isCaseClassTypeTag(typeOf[T])==false) {
      throw new com.github.raka.gqltypegen.InvalidClassTypeException("Invalid Class Provided to convert to GraphQL type.")
    }

    //First, getting package name of this class.
    imports.append("import "+typeOf[T].typeSymbol.asClass.fullName+"\n")

    //generating GQL type.
    generateGQLType(typeOf[T])

    //Removing duplicates generated types.
    //e.g. case class Foo(a:Bar, b:Bar), here bar will be generated twice, need to remove the duplicate.
    val distinctList = ListBuffer[GQLType]()
    def removeDuplicates(list: ListBuffer[GQLType]): Unit = {
      //checking for duplicate only if 2 or more items are available.
      if (list.size > 1){
          if (list.tail.filter(tp => {!(tp.tpe =:= list.head.tpe)}).size == list.tail.size) {
            //no duplicates found
            distinctList += list.head
            removeDuplicates(list.tail)
          } else {
            //duplicate found. skipping this item.
            removeDuplicates(list.tail)
          }
      } else {
        distinctList ++= list
      }
    }
    //removing duplicates
    removeDuplicates(generatedGQLTypes)

    //Generating codes.
    //val imports = "import sangria.schema._"
    //Note, current executing class is also append in name. e.g. If you are running from Main class, then entityName
    //will be Main.EntityName, need to remove Main. part.
    val entityName = typeOf[T].resultType.toString.split("\\.").last
    val className = entityName+"SchemaDefinition"
    val comments = s"//Auto Generated GraphQL Types Definition for Entity $entityName"+".\n//Generated At: "+ Calendar.getInstance.getTime+"\n"
    val generatedCodes = new StringBuilder
    generatedCodes.append(imports)
      .append("\n")
      .append(comments)
      .append("\n")
      .append("object "+className + "{\n\n")

    distinctList.foreach(gqlType => {
      generatedCodes.append(gqlType.toString)
        .append("\n")
    })

    generatedCodes.append("}")

    //Now writing generated code to file.
    val file = new File(className+".scala")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(generatedCodes.toString)
    bw.close()

    //Returning file.
    file
  }

  trait GQLTypeDefinition
  private[this] case class GQLType(typeVariableName: String, typeVariableDefinition: GQLTypeDefinition, tpe: Type) {
    override def toString: String = {
      val str = new StringBuilder
      str.append("\tlazy val ")
        .append(typeVariableName)
        .append(" = ")
        .append(typeVariableDefinition.toString)

      str.toString
    }
  }
  private[this] case class GQLObjectTypeDefinition(typeName: String, className:String, description: String, interfaces:String = "", fields: List[GQLFieldDefinition]) extends GQLTypeDefinition{
    override def toString: String = {
      val str = new StringBuilder
      str.append("ObjectType(")
        .append("\n")
        .append("\t\t\""+typeName +"\",")
        .append("\n")
        .append("\t\t\""+description+"\",")
        .append("\n")
        .append("\t\tfields[Unit, "+className+"](")
        .append("\n")
      fields.init.foreach(field => {
        str.append("\t\t\t").append(field.toString).append(",\n")
      })
      str.append("\t\t\t").append(fields.last.toString) //last entry doesnot have ","
        .append("\n")
        .append("\t\t)")
        .append("\n\t)")

      str.toString()
    }
  }
  private[this] case class GQLFieldDefinition(fieldName: String, fieldType: String, wrappingFields:List[GQLDefaultGenericType.Value] = List(), fieldResolver:String) {
    override def toString: String = {
      //need to add wrapping types if any.
      //Note - while resolving option and type only scalar value is returned, so need to add wrapping type.
      //e.g. Option[List[String]] is resolved to scalar value String, and wrapping type is (list, Option).
      val wrappedType = wrappingFields.map(_.toString).foldLeft(fieldType){
        (a, b) => b +"(" + a + ")"
      }
      "Field(\""+fieldName+"\", "+wrappedType+", resolve = "+fieldResolver+")"
    }
  }
  private[this] case class GQLEnumTypeDefinition(name:String, description:String, values: List[GQLEnumFieldDefinition]) extends GQLTypeDefinition {
    override def toString: String = {
      val str = new StringBuilder
      str.append("EnumType(")
        .append("\n")
        .append("\t\t"+name +",")
        .append("\n")
        .append("\t\t"+description+",")
        .append("\n")
        .append("\t\tList(")
        .append("\n")
      values.init.foreach(value => {
        str.append("\t\t\t").append(value.toString).append(",\n")
      })
      str.append("\t\t\t").append(values.last.toString) //last entry doesnot have ","
        .append("\n")
        .append("\t\t)")
        .append("\n\t)")

      str.toString()
    }
  }
  private[this] case class GQLEnumFieldDefinition(name: String, value:String, description:String) {
    override def toString: String = {
      //EnumValue("crm", value = Module.crm, description = Some("CRM module"))
      "EnumValue("+name+", value = "+value+", description = Some("+description+"))"
    }
  }

  //Parent FieldName - fieldName of Parent Type.
  //For example - info: Person, Person(name: String) - here, parent field name for Person is info.
  //return generated gql type variable name.
  private[this] def generateGQLType(ruType: Type, parentFieldName: String = ""): String = {

    //For type variable naming.
    val typeVariableName = ruType.resultType.toString.split("\\.").last + "ObjectType"

    //For GQL type definition.
    val className = ruType.resultType.toString.split("\\.").last
    val typeName = className + "_ObjectType"
    val typeDescription = className + " GraphQL Type Definition"

    //Iterating each member/field of model entity / case class.
    //and mapping it to GraphQL fields.
    val typeFields: List[GQLFieldDefinition] = ruType.members.collect {
      //making sure that we only get class fields.
      case m: MethodSymbol if m.isGetter => {

        //First Resolving option/list type to get scalar field.
        //i.e. List[String] - get String type, List[Option[Int]] get Int scalar type. so on...
        val resolvedScalarType = resolveOptionAndSeqType(tag = m.getter.typeSignature.resultType)

        //checking field type and resolving to appropriate GQL fields.
        resolvedScalarType._1 match {
          case tpe if (Utilities.isCaseClassTypeTag(tpe)) => {

            //First, getting package name of this class and storing it in imports.
            imports.append("import "+tpe.typeSymbol.asClass.fullName+"\n")

            //Field type is another case class. i.e. nested entity object. need to resolve type for this type as well.
            val typeVarName = generateGQLType(tpe, parentFieldName+"."+m.name.toString)

            //Now also need to return object field type for this.
            val fieldName = m.name.toString
            val gqlField = GQLFieldDefinition(fieldName = fieldName, fieldType = typeVarName, fieldResolver = "_.value."+fieldName)

            if(!resolvedScalarType._2.isEmpty){
              gqlField.copy(wrappingFields = resolvedScalarType._2)
            }else {
              gqlField
            }
          }
          case tpe if (tpe <:< typeOf[Enumeration#Value]) => {
            //Field type is Enumeration, first generating separate GQL enum Type for this enum.

            //First, getting package name of this class and storing it in imports.
            imports.append("import "+tpe.typeSymbol.asClass.fullName+"\n")

            val enumType: GQLEnumTypeDefinition = resolveEnumType(tpe, m.name.toString, parentFieldName)

            //Return Enum class name will be Foo.Value, need to remove .Value.
            //using split - to remove classloader class name.
            val enumTypeVariableName = tpe.asInstanceOf[TypeRef].resultType.toString.replace(".Value", "").split("\\.").last+"EnumType"

            //Adding the generated GQL Types to generated list.
            generatedGQLTypes += GQLType(enumTypeVariableName, enumType, tpe)

            //Now also need to return object field type for this.
            val fieldName = m.name.toString
            val gqlField = GQLFieldDefinition(fieldName = fieldName, fieldType = enumTypeVariableName, fieldResolver = "_.value."+fieldName)

            if(!resolvedScalarType._2.isEmpty){
              gqlField.copy(wrappingFields = resolvedScalarType._2)
            }else {
              gqlField
            }
          }
          case tpe@_ => {
            //Note: There can be issues in certain type package name imports for customType,
            // so may need to resolve manually.
            if(!Utilities.isAnyValType(tpe)) {
              //Getting package name of this class and storing it in imports.
              imports.append("import "+tpe.typeSymbol.asClass.fullName+"\n")
            }

            val filteredCustomScalarTypes = customScalarTypes.filter(cs => {
              //println(">>>>>>>>>>>"+ currentMirror.reflect(new cs.InputType).symbol.toType.toString)
              //matching custom scalar type with resolved scalar type.
              //currentMirror.reflect(new cs.InputType).symbol.toType =:= tpe
              cs.inputType =:= tpe
            })
            if(filteredCustomScalarTypes.isEmpty) {
              //The above type is not defined in custom scalar type. Checking for default scalar types.
              val gqlField: GQLFieldDefinition = resolveDefaultScalarTypeField(tpe, m.name.toString, parentFieldName)
              if(!resolvedScalarType._2.isEmpty){
                gqlField.copy(wrappingFields = resolvedScalarType._2)
              }else {
                gqlField
              }
            }else {
              //The above type is overridden or is defined in custom scalar type definition. applying it.
              val customScalarType = filteredCustomScalarTypes.head
              //val inTtpe = currentMirror.reflect(new customScalarType.InputType).symbol.toType
              //val outTtpe = currentMirror.reflect(new customScalarType.OutputType).symbol.toType
              val gqlField: GQLFieldDefinition = resolveCustomScalarTypeField(customScalarType.inputType, customScalarType.outputType, m.name.toString, parentFieldName, resolvedScalarType._2, customScalarType.generateConversionCode)
              if(!resolvedScalarType._2.isEmpty){
                gqlField.copy(wrappingFields = resolvedScalarType._2)
              }else {
                gqlField
              }
            }
          }
        }
      }
    }.toList

    if(typeFields.isEmpty) throw new InvalidClassException("Invalid Object is provided for Conversion.")

    val gqlObj = GQLObjectTypeDefinition(typeName = typeName, className = className, description = typeDescription, fields = typeFields)

    //Adding the generated GQL Types to generated list.
    generatedGQLTypes += GQLType(typeVariableName, gqlObj, ruType)

    //returning type variable name.
    typeVariableName
  }

  /*private[this] def generateGQLType[T <: AnyRef : TypeTag](parentFieldName: String = ""): Unit = {
    val ruType = typeOf[T]

    //For type variable naming.
    val typeVariableName = ruType.resultType.toString + "Type"

    //For GQL type definition.
    val typeName = ruType.resultType.toString + "_Type"
    val typeDescription = ruType.resultType.toString + " GraphQL Type"

    //Iterating each member/field of model entity / case class.
    //and mapping it to GraphQL fields.
    val typeFields: List[GQLFieldDefinition] = ruType.members.collect {
      //making sure that we only get class fields.
      case m: MethodSymbol if m.isGetter => {

        //First Resolving option/list type to get scalar field.
        //i.e. List[String] - get String type, List[Option[Int]] get Int scalar type. so on...
        val resolvedScalarType = resolveOptionAndSeqType(tag = m.getter.typeSignature.resultType)

        //checking field type and resolving to appropriate GQL fields.
        resolvedScalarType._1 match {
          case tpe if (isCaseClassTypeTag(tpe)) => {
            //Field type is another case class. i.e. nested entity object. need to resolve type for this type as well.
            val aaa = currentMirror.reflect(ruType.).instance
            currentMirror.runtimeClass(ruType).getClass
            generateGQLType[aaa.type](parentFieldName+"."+m.name.toString)

          }
          case tpe if (tpe <:< typeOf[Enumeration#Value]) => {
            //Field type is Enumeration, first generating separate GQL enum Type for this enum.
            val enumType: GQLEnumTypeDefinition = resolveEnumType(tpe, m.name.toString, parentFieldName)
            val enumTypeVariableName = tpe.resultType.toString+"GQLType"

            //Adding the generated GQL Types to generated list.
            generatedGQLTypes += GQLType(enumTypeVariableName, enumType)

            //Now also need to return object field type for this.
            val fieldName = m.name.toString
            val gqlField = GQLFieldDefinition(fieldName = fieldName, fieldType = enumTypeVariableName, fieldResolver = "_.value."+fieldName)

            if(!resolvedScalarType._2.isEmpty){
              gqlField.copy(wrappingFields = resolvedScalarType._2)
            }else {
              gqlField
            }
          }
          case tpe@_ => {
            val filteredCustomScalarTypes = customScalarTypes.filter(cs => {
              //matching custom scalar type with resolved scalar type.
              currentMirror.reflect(new cs.InputType).symbol.toType =:= tpe
            })
            if(filteredCustomScalarTypes.isEmpty) {
              //The above type is not defined in custom scalar type. Checking for default scalar types.
              val gqlField: GQLFieldDefinition = resolveDefaultScalarTypeField(tpe, m.name.toString, parentFieldName)
              if(!resolvedScalarType._2.isEmpty){
                gqlField.copy(wrappingFields = resolvedScalarType._2)
              }else {
                gqlField
              }
            }else {
              //The above type is overridden or is defined in custom scalar type definition. applying it.
              val customScalarType = filteredCustomScalarTypes.head
              val inTtpe = currentMirror.reflect(new customScalarType.InputType).symbol.toType
              val outTtpe = currentMirror.reflect(new customScalarType.OutputType).symbol.toType
              val gqlField: GQLFieldDefinition = resolveCustomScalarTypeField(inTtpe, outTtpe, m.name.toString, parentFieldName, customScalarType.generateConversionCode)
              if(!resolvedScalarType._2.isEmpty){
                gqlField.copy(wrappingFields = resolvedScalarType._2)
              }else {
                gqlField
              }
            }
          }
        }
        null
      }
    }.toList

    if(typeFields.isEmpty) throw new InvalidClassException("Invalid Object is provided for Conversion.")
    ""
  }*/

  private def resolveDefaultScalarTypeField(tpe: Type, fieldName: String, parentFieldName: String): GQLFieldDefinition = {
    tpe match {
      case t if t =:= typeOf[Int] || t =:= typeOf[Integer]=> {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.INT.toString
        val resolver = "_.value."+fieldName
        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[Float] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.FLOAT.toString
        val resolver = "_.value."+fieldName
        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[Boolean] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BOOLEAN.toString
        val resolver = "_.value."+fieldName
        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[String] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.STRING.toString
        val resolver = "_.value."+fieldName
        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[Long] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.LONG.toString
        val resolver = "_.value."+fieldName
        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[BigDecimal] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BIGDECIMAL.toString
        val resolver = "_.value."+fieldName
        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[BigInt] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BIGINT.toString
        val resolver = "_.value."+fieldName
        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case _ => throw new com.github.raka.gqltypegen.InvalidScalarTypeException(if(parentFieldName.isEmpty)fieldName else parentFieldName+"."+fieldName, "Type is not scalar type(Int, Float, Boolean, String, Long, BigInt, BigDecimal). Please, define custom scalar definition for this type - "+ tpe.resultType.toString)
    }
  }

  //What it does?
  //e.g. you have custom type but with generic wrapper - option/list. - List[Option[JsValue].
  // In this case, we need to iterate through list and the resolve option and finally convert the JsValue to user defined
  // type. Then, below method will resolve such situation and produce codes.
  //Note: resolveCode name - resolve(). This name is defined in - com.github.raka.gqltypegen.CustomScalarType class - generateConversionCode() method.
  private def resolveCustomTypeWrapper(value: String, wrappingTypes: List[GQLDefaultGenericType.Value], tabs: String = ""): String = {
    val result: StringBuilder = new StringBuilder
    if (wrappingTypes.isEmpty) "resolve(" + value + ")"
    else {
      //Using .last, because Wrapping types is in reverse order in list. e.g. List[Option[String]] will be (Option, List)
      wrappingTypes.last match {
        case GQLDefaultGenericType.OPTION => {
          result.append(tabs + value + " match {\n")
            .append(tabs + "\tcase Some(v) => {\n")
            .append(tabs + "\t\tval res = ")
            .append(resolveCustomTypeWrapper(value = "v", wrappingTypes = wrappingTypes.init, tabs = tabs + "\t\t").trim)
            .append("\n")
            .append(tabs + "\t\tSome(res)\n")
            .append(tabs + "\t}\n")
            .append(tabs + "\tcase None => None\n")
            .append(tabs + "}\n")

          result.toString()
        }
        case GQLDefaultGenericType.LIST => {
          result.append(tabs + value + ".map(v => {\n")
            .append(tabs + "\t"+resolveCustomTypeWrapper(value = "v", wrappingTypes = wrappingTypes.init, tabs = tabs + "\t"))
            .append("\n")
            .append(tabs +"\t"+ "})")
          result.toString()
        }
        case _ => "resolve(" + value + ")"
      }
    }
  }

  private def resolveCustomScalarTypeField(inputTpe: Type, outType: Type, fieldName: String, parentFieldName: String, wrappingTypes:List[GQLDefaultGenericType.Value], resolverCode: String): GQLFieldDefinition = {
    outType match {
      case t if t =:= typeOf[Int] || t =:= typeOf[Integer]=> {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.INT.toString
        val resolver = "{ v => {\n" +
          resolverCode+"\n"+
          resolveCustomTypeWrapper(value = "v.value."+fieldName, wrappingTypes = wrappingTypes, tabs="\t\t\t\t")+
          "\n\t\t\t}}"

        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[Float] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.FLOAT.toString
        /*val resolver = "{ v => {\n" +
          resolverCode +
          "\n\t\t\t\tresolve(v.value." + fieldName +")"+
          "\n\t\t\t}}"*/
        val resolver = "{ v => {\n" +
          resolverCode+"\n"+
          resolveCustomTypeWrapper(value = "v.value."+fieldName, wrappingTypes = wrappingTypes, tabs="\t\t\t\t")+
          "\n\t\t\t}}"

        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[Boolean] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BOOLEAN.toString
        /*val resolver = "{ v => {\n" +
          resolverCode +
          "\n\t\t\t\tresolve(v.value." + fieldName +")"+
          "\n\t\t\t}}"*/
        val resolver = "{ v => {\n" +
          resolverCode+"\n"+
          resolveCustomTypeWrapper(value = "v.value."+fieldName, wrappingTypes = wrappingTypes, tabs="\t\t\t\t")+
          "\n\t\t\t}}"

        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[String] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.STRING.toString
        /*val resolver = "{ v => {\n" +
          resolverCode +
          "\n\t\t\t\tresolve(v.value." + fieldName +")"+
          "\n\t\t\t}}"*/
        val resolver = "{ v => {\n" +
          resolverCode+"\n"+
          resolveCustomTypeWrapper(value = "v.value."+fieldName, wrappingTypes = wrappingTypes, tabs="\t\t\t\t")+
          "\n\t\t\t}}"

        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[Long] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.LONG.toString
        /*val resolver = "{ v => {\n" +
          resolverCode +
          "\n\t\t\t\tresolve(v.value." + fieldName +")"+
          "\n\t\t\t}}"*/
        val resolver = "{ v => {\n" +
          resolverCode+"\n"+
          resolveCustomTypeWrapper(value = "v.value."+fieldName, wrappingTypes = wrappingTypes, tabs="\t\t\t\t")+
          "\n\t\t\t}}"

        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[BigDecimal] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BIGDECIMAL.toString
        /*val resolver = "{ v => {\n" +
          resolverCode +
          "\n\t\t\t\tresolve(v.value." + fieldName +")"+
          "\n\t\t\t}}"*/
        val resolver = "{ v => {\n" +
          resolverCode+"\n"+
          resolveCustomTypeWrapper(value = "v.value."+fieldName, wrappingTypes = wrappingTypes, tabs="\t\t\t\t")+
          "\n\t\t\t}}"

        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case t if t =:= typeOf[BigInt] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BIGINT.toString
        /*val resolver = "{ v => {\n" +
          resolverCode +
          "\n\t\t\t\tresolve(v.value." + fieldName +")"+
          "\n\t\t\t}}"*/
        val resolver = "{ v => {\n" +
          resolverCode+"\n"+
          resolveCustomTypeWrapper(value = "v.value."+fieldName, wrappingTypes = wrappingTypes, tabs="\t\t\t\t")+
          "\n\t\t\t}}"

        GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver)
      }
      case _ => throw new com.github.raka.gqltypegen.InvalidScalarTypeException(if(parentFieldName.isEmpty)fieldName else parentFieldName+"."+fieldName, "Type is not scalar type(Int, Float, Boolean, String, Long, BigInt, BigDecimal). Please, define custom scalar definition for this type - "+ inputTpe.resultType.toString)
    }
  }

  private def resolveEnumType(tpe: Type, fieldName: String, parentFieldName: String): GQLEnumTypeDefinition = {
    //Getting list of all defined value of the Enum.
    val enumValues = tpe.asInstanceOf[TypeRef]
      .pre
      .members
      .filter(s => {!s.isMethod && s.typeSignature.baseType(tpe.typeSymbol) =:= tpe})
      .toList
      .map(_.name.toString.trim)

    //Return Enum class name will be Foo.Value, need to remove .Value.
    //using split - to remove classloader class name.
    val enumClassName = tpe.asInstanceOf[TypeRef].resultType.toString.replace(".Value", "").split("\\.").last

    val typeName = "\""+enumClassName + "_EnumType" + "\""
    val typeDescription = "Some(\"" + enumClassName +" Enumeration Type Definition.\")"

    val enumFields = enumValues.map(name => {
      val gqlEnumValue = enumClassName+"."+name
      val description = "Some(\"Enum value "+name+" for Enum class "+enumClassName+"\")"
      GQLEnumFieldDefinition(name, gqlEnumValue, description)
    })

    GQLEnumTypeDefinition(typeName, typeDescription, enumFields)
  }

  //Resolve Single/Nested Option/Seq type and get scalar type with its wrapping types.
  //For example: Option[List[String]] will resolve to scalar String type and wrapping types as (ListType, OptionType).
  //Note: Wrapping value is returned in reverse. in above case, list contain - (List, Option)
  //In GQL specification this is translated into: OptionType(ListType(String)).
  //In addition, note that ListType and OptionType name is given as GQL type specification.
  //Returns - scalar type and list of wrapping option/list type as string value.
  def resolveOptionAndSeqType(wrappingTypes: List[GQLDefaultGenericType.Value] = List(), tag: Type): (Type, List[GQLDefaultGenericType.Value]) = {
    tag match {
      case x if x <:< typeOf[Option[Any]] => {
        val typeArgument = tag.typeArgs
        val wTypes = GQLDefaultGenericType.OPTION :: wrappingTypes
        resolveOptionAndSeqType(wTypes, typeArgument.head)
      }
      case x if x <:< typeOf[Seq[Any]] => {
        val typeArgument = tag.typeArgs
        val wTypes = GQLDefaultGenericType.LIST :: wrappingTypes
        resolveOptionAndSeqType(wTypes, typeArgument.head)
      }
      case _ => (tag, wrappingTypes)
    }
  }

}

//Output Scalar Type must be - Int, Float, Boolean, String, Long, BigDecimal and BigInt.
//Note giving custom value to Other nested Entity Type/case class or Enum type is not supported.
abstract class com.github.raka.gqltypegen.CustomScalarType[T: TypeTag,U: TypeTag] {

  val inputType: Type = typeOf[T]
  val outputType: Type = typeOf[U]

  private[this] def isValidOutputType = implicitly[TypeTag[U]].tpe match {
    case v if v =:= typeOf[Int] || v =:= typeOf[Integer]=> true
    case v if v =:= typeOf[Float] => true
    case v if v =:= typeOf[Boolean] => true
    case v if v =:= typeOf[String] => true
    case v if v =:= typeOf[Long] => true
    case v if v =:= typeOf[BigDecimal] => true
    case v if v =:= typeOf[BigInt] => true
    case _ => false
  }

  require(isValidOutputType, "Invalid Output Type " + outputType.resultType.toString + " provided. Output Type must be Scalar - Int, Float, Boolean, String, Long, BigInt, BigDecimal")

  //Default input value is required in generateConversionCode method to call typeConversionCode() method.
  private[this] val defaultInputValue: T = null.asInstanceOf[T]

  //This method need to be implemented by user to provide.
  //i.e. user must provide conversion code from input to output type.
  //Note: Param name must be input - if changed change resolve param text as well in generateConversionCOde().
  def typeConversionCode(input: T) : Tree

  //Generate string format code for conversion from input to output type.
  def generateConversionCode: String = {
    val inputTypeName = inputType.resultType.toString
    val outputTypeName = outputType.resultType.toString
    //val inputParam = currentMirror.reflect(implicitly[ClassTag[InputType]]).instance.asInstanceOf[InputType]
    val code = "\t\t\t\tdef resolve(input: "+inputTypeName+"): "+outputTypeName+" = "+showCode(typeConversionCode(defaultInputValue)).split("\n").map("\t\t\t\t"+_).mkString("\n").trim
    code
  }
}

//Names are according to GQL specification for scalar types.
//Builtin GraphQL scalar type = IntType :: FloatType :: BooleanType :: StringType
//Buildin Sangria scalar type = LongType :: BigIntType :: BigDecimalType
//todo IDtype
object GQLDefaultScalarType extends Enumeration {
  val INT = Value("IntType")
  val FLOAT = Value("FloatType")
  val BOOLEAN = Value("BooleanType")
  val STRING = Value("StringType")
  val LONG = Value("LongType")
  val BIGDECIMAL = Value("BigDecimalType")
  val BIGINT = Value("BigIntType")
}

object Utilities {

  //Create Parameterless constructor instance of a Class.
  def createInstanceOfClass[T : TypeTag] : T = {
    val tpe: Type = typeOf[T]
    val symbol: Symbol = tpe.members.filter(s => s.isMethod && s.asMethod.isConstructor).iterator.next()
    val typeSignature: Type = symbol.typeSignature
    val methodSymbol = symbol.asMethod
    val classSymbol = tpe.typeSymbol.asClass
    val constructor = currentMirror.reflectClass(classSymbol).reflectConstructor(methodSymbol)
    constructor().asInstanceOf[T]
  }

  //Check if given TypeTag is a case class or not.
  def isCaseClassTypeTag(tTag: Type): Boolean = {
    val symbol = tTag.typeSymbol
    symbol.asClass.isCaseClass
  }

  //Check is given TypeTag type is anyval or not.
  def isAnyValType(tpe: Type) = tpe match {
    case v if v =:= typeOf[Int] || v =:= typeOf[Integer]=> true
    case v if v =:= typeOf[Float] => true
    case v if v =:= typeOf[Boolean] => true
    case v if v =:= typeOf[String] => true
    case v if v =:= typeOf[Long] => true
    case v if v =:= typeOf[BigDecimal] => true
    case v if v =:= typeOf[BigInt] => true
    case _ => false
  }
}

//Other Default types in sangria-graphql.
//You can add other generic types here.
object GQLDefaultGenericType extends Enumeration {
  val LIST = Value("ListType")
  val OPTION = Value("OptionType")
}

//Throw this exception in case client request to convert invalid entity object into GraphQL Type.
//e.g. String, Number etc. as valid AnyRef but is not valid entity class. In other word, a class must be a case class
//to be valid.
class com.github.raka.gqltypegen.InvalidClassTypeException(msg: String) extends RuntimeException(msg)

//Throw this exception if a field type is not a valid scalar type or
//custom scalar type conversion is not defined for it. For example, DateTime, JsValue is not valid scalar type unless
//you provide custom scalar conversion. Currently supported scalar type is - Boolean, Byte, Short, Int, Long, Float,
//Double, Char, String. You must provide custom scalar type for other types except case class / entity type.
class com.github.raka.gqltypegen.InvalidScalarTypeException(scalarType: String, msg: String) extends RuntimeException("Field: "+scalarType+", Reason: "+msg)


//Issues:
//If customType is generic. e.g. you have a generic class type, currently, this is not supported.
//e.g. Person[T], val info : Person[Programmer], this custom type will not work.
//todo - maybe this can be solved with custom type definition.*/
