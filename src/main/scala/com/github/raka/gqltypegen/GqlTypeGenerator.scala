package com.github.raka.gqltypegen

import java.io.{BufferedWriter, File, FileWriter, InvalidClassException}
import java.util.Calendar

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

/**
  * A Scala Case Class to GraphQL Type Converter.
  * Note: Type generator will work only for case class model object.
  *
  * Todo - Implement interfaces in type.
  *
  * Note: Generating Output and Input Types is not ThreadSafe.
  *
  * Created by Ranjit Kaliraj on 11/6/17.
  */

class GqlTypeGenerator private(){

  //GQL only knows, certain types, other type should be provide as custom type, with manual resolve code.
  //Hold custom user defined scalar types. Applicable for generating Query type only.
  private[this] val customScalarTypes = ListBuffer[CustomScalarType[_<:Any, _<:Any]]()

  //GQL allow implicit conversion in certain types. e.g. JsValue, because of Json Marshalling.
  //This list contains list of user-provided non-allowed scalar types, for which custom scalar type is not required not be defined.
  //Applicable for generating both Query and Mutation type.
  //Note: Use this only if you are sure that GQL has implicit type conversion for given non-scalar type.
  private[this] val allowedNonScalarInputTypes = ListBuffer[AllowedNonScalarInputType[_<:Any, _<:Any]]()

  //Holds package name of all found classes/case classes for import (except primitive.)
  //Importing sangria libraries by default.
  private[this] val imports = new StringBuilder

  //Holds all generated GQL types.
  private[this] val generatedGQLTypes = ListBuffer[GQLType]()

  //Add custom defined scalar types to the list.
  def addCustomScalarTypeDef[A <: Any,B <: Any](cType: CustomScalarType[A,B]): Unit = {
    customScalarTypes.+=(cType)
  }

  //Allow non-scalar type. This ensures that when non-scalar or undefined custom type is encountered, if
  //it is allowed in this list, it will be considered as scalar type.
  //Note: Only valid for InputType or Mutation Type and not for Query type.
  def allowNonScalarType[A <: Any, B <: Any](cType: AllowedNonScalarInputType[A, B]): Unit = {
    allowedNonScalarInputTypes.+=(cType)
  }

  //Generate Graphql Types for Query.
  def generateQueryTypes[T <: AnyRef : TypeTag]: File = {
    implicit val action = GQLAction.QUERY
    generate[T](generateOutputGQLType)
  }

  //Generate Graphql Types for Mutation.
  def generateMutationTypes[T <: AnyRef : TypeTag]: File = {
    implicit val action = GQLAction.MUTATION
    generate[T](generateInputGQLType)
  }

  //Generate graphql type.
  private[this] def generate[T <: AnyRef : TypeTag](generateFunction: (Type, String) => String)(implicit action: GQLAction.Value): File = {

    //First, emptying any previous generated results.
    generatedGQLTypes.clear()
    imports.clear()
    imports.append("import sangria.schema._\n")

    //GQL Type should not be generated for non-case class types.
    if(GQLUtils.isCaseClassTypeTag(typeOf[T])==false) {
      throw new InvalidClassTypeException("Invalid Class Provided to convert to GraphQL type.")
    }

    //First, getting package name of this class for import.
    imports.append("import "+typeOf[T].typeSymbol.asClass.fullName+"\n")

    //generating GQL type.
    //generateOutputGQLType(typeOf[T])
    generateFunction(typeOf[T], "")

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

    //Generating and writing codes to file.
    //Note, current executing class is also append in name. e.g. If you are running from Main class, then entityName
    //will be Main.EntityName, need to remove Main. part.
    val entityName = typeOf[T].resultType.toString.split("\\.").last
    val className = action match {
      case GQLAction.QUERY => entityName+"QueryTypeDefinition"
      case GQLAction.MUTATION => entityName+"MutationTypeDefinition"
    }
    val comments = s"\n//Auto Generated GraphQL Types Definition for Entity $entityName"+".\n//Generated At: "+ Calendar.getInstance.getTime+"\n"
    val generatedCodes = new StringBuilder
    generatedCodes.append(imports.mkString.split("\n").distinct.mkString("\n"))
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

  //Generate Output GQL Types for Query.
  //Parent FieldName - fieldName of Parent Type.
  //For example - info: Person, Person(name: String) - here, parent field name for Person is info.
  //This method generate gql types and write to list and return generated gql type variable name.
  private[this] def generateOutputGQLType(ruType: Type, parentFieldName: String = ""): String = {

    //For type variable naming.
    val typeVariableName = ruType.resultType.toString.split("\\.").last + "ObjectType"

    //For GQL type definition.
    val className = ruType.resultType.toString.split("\\.").last
    val typeName = className + "_ObjectType"
    val typeDescription = className + " GraphQL Type Definition for Query"

    //Iterating each member/field of model entity / case class.
    //and mapping it to GraphQL fields.
    val typeFields: List[GQLFieldDefinition] = ruType.members.collect {
      //making sure that we only get class fields.
      case m: MethodSymbol if m.isGetter => {

        val fieldName = m.name.toString match {
          case "type" => "`type`"
          case e@_ => e
        }

        //First Resolving option/list type to get scalar field.
        //i.e. List[String] - get String type, List[Option[Int]] get Int scalar type. so on...
        val resolvedScalarType = resolveOptionAndSeqType(tag = m.getter.typeSignature.resultType, action = GQLAction.QUERY)

        //checking field type and resolving to appropriate GQL fields.
        resolvedScalarType._1 match {
          case tpe if (GQLUtils.isCaseClassTypeTag(tpe)) => {

            //First, getting package name of this class and storing it in imports.
            imports.append("import "+tpe.typeSymbol.asClass.fullName+"\n")

            //Field type is another case class. i.e. nested entity object. need to resolve type for this type as well.
            val typeVarName = generateOutputGQLType(tpe, parentFieldName+"."+fieldName)

            //Now also need to return object field type for this.
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
            imports.append("import "+tpe.asInstanceOf[TypeRef].pre.typeSymbol.asClass.fullName+"\n")

            if(resolvedScalarType._2.isEmpty)
              generateEnumGQLType(tpe = tpe, fieldName = fieldName,generatedTypes=generatedGQLTypes, action = GQLAction.QUERY).left.get
            else
              generateEnumGQLType(tpe, fieldName, resolvedScalarType._2, generatedGQLTypes, GQLAction.QUERY).left.get
          }
          case tpe@_ => {
            //Note: There can be issues in certain type package name imports for customType,
            // so may need to resolve manually.
            if(!GQLUtils.isAnyValType(tpe)) {
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
              val gqlField: GQLFieldDefinition = resolveDefaultScalarTypeField(tpe, fieldName, parentFieldName, GQLAction.QUERY).left.get
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
              val gqlField: GQLFieldDefinition = resolveCustomScalarTypeField(customScalarType.inputType, customScalarType.outputType, fieldName, parentFieldName, resolvedScalarType._2, customScalarType.generateConversionCode)
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

  //Generate Input GQL Types for Mutation.
  private[this] def generateInputGQLType(ruType: Type, parentFieldName: String = ""): String = {

    //For type variable naming.
    val typeVariableName = ruType.resultType.toString.split("\\.").last + "InputObjectType"

    //For GQL type definition.
    val className = ruType.resultType.toString.split("\\.").last
    val typeName = className + "_InputObjectType"
    val typeDescription = className + " GraphQL Input Type Definition for Mutation"

    //Iterating each member/field of model entity / case class.
    //and mapping it to GraphQL fields.
    val typeFields: List[GQLInputFieldDefinition] = ruType.members.collect {
      //making sure that we only get class fields.
      case m: MethodSymbol if m.isGetter => {

        val fieldName = m.name.toString match {
          case "type" => "`type`"
          case e@_ => e
        }
        //First Resolving option/list type to get scalar field.
        //i.e. List[String] - get String type, List[Option[Int]] get Int scalar type. so on...
        val resolvedScalarType = resolveOptionAndSeqType(tag = m.getter.typeSignature.resultType, action = GQLAction.QUERY)

        //checking field type and resolving to appropriate GQL fields.
        resolvedScalarType._1 match {
          case tpe if (GQLUtils.isCaseClassTypeTag(tpe)) => {

            //First, getting package name of this class and storing it in imports.
            imports.append("import " + tpe.typeSymbol.asClass.fullName + "\n")

            //Field type is another case class. i.e. nested entity object. need to resolve type for this type as well.
            val typeVarName = generateInputGQLType(tpe, parentFieldName + "." + fieldName)

            //Now also need to return object field type for this.
            val gqlField = GQLInputFieldDefinition(fieldName = fieldName, fieldType = typeVarName, description = fieldName + " input field")

            if (!resolvedScalarType._2.isEmpty) {
              gqlField.copy(wrappingFields = resolvedScalarType._2)
            } else {
              gqlField
            }
          }
          case tpe if (tpe <:< typeOf[Enumeration#Value]) => {
            //Field type is Enumeration, first generating separate GQL enum Type for this enum.

            //First, getting package name of this class and storing it in imports.
            imports.append("import " + tpe.asInstanceOf[TypeRef].pre.typeSymbol.asClass.fullName + "\n")

            if (resolvedScalarType._2.isEmpty)
              generateEnumGQLType(tpe = tpe, fieldName = fieldName, generatedTypes = generatedGQLTypes, action = GQLAction.MUTATION).right.get
            else
              generateEnumGQLType(tpe, fieldName, resolvedScalarType._2, generatedGQLTypes, GQLAction.MUTATION).right.get
          }
          case tpe@_ => {
            //Note: There can be issues in certain type package name imports for customType,
            // so may need to resolve manually.
            if (!GQLUtils.isAnyValType(tpe)) {
              //Getting package name of this class and storing it in imports.
              imports.append("import " + tpe.typeSymbol.asClass.fullName + "\n")
            }
            //The above type is not defined in custom scalar type. Checking for default scalar types.
            val gqlField: GQLInputFieldDefinition = resolveDefaultScalarTypeField(tpe, fieldName, parentFieldName, GQLAction.MUTATION).right.get
            if (!resolvedScalarType._2.isEmpty) {
              gqlField.copy(wrappingFields = resolvedScalarType._2)
            } else {
              gqlField
            }
          }
        }
      }
    }.toList

    if (typeFields.isEmpty) throw new InvalidClassException("Invalid Object is provided for Conversion.")

    val gqlObj = GQLInputObjectTypeDefinition(typeName = typeName, className = className, description = typeDescription, fields = typeFields)

    //Adding the generated GQL Types to generated list.
    generatedGQLTypes += GQLType(typeVariableName, gqlObj, ruType)

    //returning type variable name.
    typeVariableName
  }

  //Generate Enum GQL Types for both Query and Mutation.
  private[this] def generateEnumGQLType(tpe:Type, fieldName: String, wrappingTypes: List[GQLDefaultGenericType.Value] = List(), generatedTypes: ListBuffer[GQLType], action: GQLAction.Value): Either[GQLFieldDefinition, GQLInputFieldDefinition] = {
    val enumType: GQLEnumTypeDefinition = resolveEnumType(tpe)

    //Return Enum class name will be Foo.Value, need to remove .Value.
    //using split - to remove classloader class name.
    //val enumTypeVariableName = tpe.asInstanceOf[TypeRef].resultType.toString.replace(".Value", "").split("\\.").last+"EnumType"
    val enumTypeVariableName = tpe.asInstanceOf[TypeRef].pre.typeSymbol.asClass.name+"EnumType"

    //Adding the generated GQL Types to generated list.
    generatedTypes += GQLType(enumTypeVariableName, enumType, tpe)

    action match {
      case GQLAction.QUERY => Left(GQLFieldDefinition(fieldName = fieldName, fieldType = enumTypeVariableName, wrappingFields = wrappingTypes, fieldResolver = "_.value."+fieldName))
      case GQLAction.MUTATION => Right(GQLInputFieldDefinition(fieldName = fieldName, fieldType = enumTypeVariableName, wrappingFields = wrappingTypes, description = fieldName + " Enum Value."))
    }
  }

  //Resolve and create field definition for default scalar type - Int, Float, Boolean, String, Long, BigDecimal and BigInt.
  //For both Query and Mutation Type.
  private[this] def resolveDefaultScalarTypeField(tpe: Type, fieldName: String, parentFieldName: String, action: GQLAction.Value): Either[GQLFieldDefinition, GQLInputFieldDefinition] = {
    tpe match {
      case t if t =:= typeOf[Int] || t =:= typeOf[Integer]=> {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.INT.toString
        action match {
          case GQLAction.QUERY => {
            val resolver = "_.value."+fieldName
            Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver))
          }
          case GQLAction.MUTATION => {
            Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType, description = fName + "of type Integer"))
          }
        }
      }
      case t if t =:= typeOf[Float] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.FLOAT.toString
        action match {
          case GQLAction.QUERY => {
            val resolver = "_.value."+fieldName
            Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver))
          }
          case GQLAction.MUTATION => {
            Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType, description = fName + "of type Float"))
          }
        }
      }
      case t if t =:= typeOf[Boolean] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BOOLEAN.toString
        action match {
          case GQLAction.QUERY => {
            val resolver = "_.value."+fieldName
            Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver))
          }
          case GQLAction.MUTATION => {
            Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType, description = fName + "of type Boolean"))
          }
        }
      }
      case t if t =:= typeOf[String] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.STRING.toString
        action match {
          case GQLAction.QUERY => {
            val resolver = "_.value."+fieldName
            Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver))
          }
          case GQLAction.MUTATION => {
            Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType, description = fName + "of type String"))
          }
        }
      }
      case t if t =:= typeOf[Long] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.LONG.toString
        action match {
          case GQLAction.QUERY => {
            val resolver = "_.value."+fieldName
            Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver))
          }
          case GQLAction.MUTATION => {
            Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType, description = fName + "of type Long"))
          }
        }
      }
      case t if t =:= typeOf[BigDecimal] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BIGDECIMAL.toString
        action match {
          case GQLAction.QUERY => {
            val resolver = "_.value."+fieldName
            Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver))
          }
          case GQLAction.MUTATION => {
            Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType, description = fName + "of type BigDecimal"))
          }
        }
      }
      case t if t =:= typeOf[BigInt] => {
        val fName = StringUtil.snakecase(fieldName)
        val fieldType = GQLDefaultScalarType.BIGINT.toString
        action match {
          case GQLAction.QUERY => {
            val resolver = "_.value."+fieldName
            Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType, fieldResolver = resolver))
          }
          case GQLAction.MUTATION => {
            Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType, description = fName + "of type BigInt"))
          }
        }
      }
      case _ => {
        //Checking if type is allowed for scalar type or not - as default implicit.
        val filteredAllowedType = allowedNonScalarInputTypes.filter(t => {
          t.allowedTo =:= tpe
        })
        val fName = StringUtil.snakecase(fieldName)
        if (filteredAllowedType.isEmpty) {
          throw new InvalidScalarTypeException(if (parentFieldName.isEmpty) fieldName else parentFieldName + "." + fieldName, "Type is not scalar type(Int, Float, Boolean, String, Long, BigInt, BigDecimal). Please, define custom scalar definition for this type - " + tpe.resultType.toString)
        } else {
          val fieldType = filteredAllowedType.head.allowedAs match {
            case v if v =:= typeOf[Int] || v =:= typeOf[Integer] => GQLDefaultScalarType.INT
            case v if v =:= typeOf[Float] => GQLDefaultScalarType.FLOAT
            case v if v =:= typeOf[Boolean] => GQLDefaultScalarType.BOOLEAN
            case v if v =:= typeOf[String] => GQLDefaultScalarType.STRING
            case v if v =:= typeOf[Long] => GQLDefaultScalarType.LONG
            case v if v =:= typeOf[BigDecimal] => GQLDefaultScalarType.BIGDECIMAL
            case v if v =:= typeOf[BigInt] => GQLDefaultScalarType.BIGINT
            case _ => throw new InvalidScalarTypeException(if (parentFieldName.isEmpty) fieldName else parentFieldName + "." + fieldName, "Type is not scalar type(Int, Float, Boolean, String, Long, BigInt, BigDecimal). Please, define custom scalar definition for this type - " + tpe.resultType.toString)
          }
          action match {
            case GQLAction.QUERY => {
              val resolver = "_.value." + fieldName
              Left(GQLFieldDefinition(fieldName = fName, fieldType = fieldType.toString, fieldResolver = resolver))
            }
            case GQLAction.MUTATION => {
              Right(GQLInputFieldDefinition(fieldName = fName, fieldType = fieldType.toString, description = fName + "of type BigInt"))
            }
          }
        }
      }
    }
  }

  //What it does? - For Query Type.
  //e.g. you have custom type but with generic wrapper - option/list. - List[Option[JsValue].
  // In this case, we need to iterate through list and the resolve option and finally convert the JsValue to user defined
  // type. Then, below method will resolve such situation and produce codes.
  //Note: resolveCode name - resolve(). This name is defined in - CustomScalarType class - generateConversionCode() method.
  private[this] def resolveCustomTypeWrapper(value: String, wrappingTypes: List[GQLDefaultGenericType.Value], tabs: String = ""): String = {
    val result: StringBuilder = new StringBuilder
    if (wrappingTypes.isEmpty) "resolve(" + value + ")"
    else {
      //Using .last, because Wrapping types is in reverse order in list. e.g. List[Option[String]] will be (Option, List)
      wrappingTypes.last match {
        case GQLDefaultGenericType.OPTION => {
          result.append(tabs + value + " match {\n")
            .append(tabs + "\tcase Some(p) => {\n")
            .append(tabs + "\t\tval res = ")
            .append(resolveCustomTypeWrapper(value = "p", wrappingTypes = wrappingTypes.init, tabs = tabs + "\t\t").trim)
            .append("\n")
            .append(tabs + "\t\tSome(res)\n")
            .append(tabs + "\t}\n")
            .append(tabs + "\tcase None => None\n")
            .append(tabs + "}")

          result.toString()
        }
        case GQLDefaultGenericType.LIST => {
          result.append(tabs + value + ".map(q => {\n")
            .append(tabs + "\t"+resolveCustomTypeWrapper(value = "q", wrappingTypes = wrappingTypes.init, tabs = tabs + "\t"))
            .append("\n")
            .append(tabs + "})")
          result.toString()
        }
        case _ => "resolve(" + value + ")"
      }
    }
  }

  //Resolve and create type for custom scalar types as user defined for Query Type only.
  private[this] def resolveCustomScalarTypeField(inputTpe: Type, outType: Type, fieldName: String, parentFieldName: String, wrappingTypes:List[GQLDefaultGenericType.Value], resolverCode: String): GQLFieldDefinition = {
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
      case _ => throw new InvalidScalarTypeException(if(parentFieldName.isEmpty)fieldName else parentFieldName+"."+fieldName, "Type is not scalar type(Int, Float, Boolean, String, Long, BigInt, BigDecimal). Please, define custom scalar definition for this type - "+ inputTpe.resultType.toString)
    }
  }

  //Create and write Enum type for Enumeration objects.
  private[this] def resolveEnumType(tpe: Type): GQLEnumTypeDefinition = {
    //Getting list of all defined value of the Enum.
    val enumValues = tpe.asInstanceOf[TypeRef]
      .pre
      .members
      .filter(s => {!s.isMethod && s.typeSignature.baseType(tpe.typeSymbol) =:= tpe})
      .toList
      .map(_.name.toString.trim)

    //Return Enum class name will be Foo.Value, need to remove .Value.
    //using split - to remove classloader class name.
    //val enumClassName = tpe.asInstanceOf[TypeRef].resultType.toString.replace(".Value", "").split("\\.").last
    val enumClassName = tpe.asInstanceOf[TypeRef].pre.typeSymbol.asClass.name.toString

    val typeName = "\""+enumClassName + "_EnumType" + "\""
    val typeDescription = "Some(\"" + enumClassName +" Enumeration Type Definition.\")"

    val enumFields = enumValues.map(name => {
      val gqlEnumValue = enumClassName+"."+name
      val description = "Some(\"Enum value "+name+" for Enum class "+enumClassName+"\")"
      GQLEnumFieldDefinition("\""+name+"\"", gqlEnumValue, description)
    })

    GQLEnumTypeDefinition(typeName, typeDescription, enumFields)
  }

  /*
   * Resolve Single/Nested Option/Seq type and get scalar type with its wrapping types for both Mutation and Query Action.
   * For example: Option[List[String]] will resolve to scalar String type and wrapping types as (ListType, OptionType).
   * Note: Wrapping value is returned in reverse. in above case, list contain - (List, Option)
   * In GQL specification this is translated into: OptionType(ListType(String)).
   * In addition, note that ListType and OptionType name is given as GQL type specification.
   * Returns - scalar type and list of wrapping option/list type as string value.
   */
  private[this] def resolveOptionAndSeqType(wrappingTypes: List[GQLDefaultGenericType.Value] = List(), tag: Type, action: GQLAction.Value): (Type, List[GQLDefaultGenericType.Value]) = {
    tag match {
      case x if x <:< typeOf[Option[Any]] => {
        val typeArgument = tag.typeArgs
        val wTypes = action match {
          case GQLAction.QUERY => GQLDefaultGenericType.OPTION :: wrappingTypes
          case GQLAction.MUTATION => GQLDefaultGenericType.INPUT_OPTION :: wrappingTypes
        }
        resolveOptionAndSeqType(wTypes, typeArgument.head, action)
      }
      case x if x <:< typeOf[Seq[Any]] => {
        val typeArgument = tag.typeArgs
        val wTypes = action match {
          case GQLAction.QUERY => GQLDefaultGenericType.LIST :: wrappingTypes
          case GQLAction.MUTATION => GQLDefaultGenericType.INPUT_LIST :: wrappingTypes
        }
        resolveOptionAndSeqType(wTypes, typeArgument.head, action)
      }
      case _ => (tag, wrappingTypes)
    }
  }

}


object GqlTypeGenerator {
  def getInstance = new GqlTypeGenerator
}