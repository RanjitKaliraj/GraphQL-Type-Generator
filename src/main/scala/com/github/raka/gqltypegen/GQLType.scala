package com.github.raka.gqltypegen

import scala.reflect.runtime.universe._

/**
  * The following case classes holds resolved scala class to graphql type information.
  *
  * Created by Ranjit Kaliraj on 11/6/17.
  */

trait GQLTypeDefinition

case class GQLType(typeVariableName: String, typeVariableDefinition: GQLTypeDefinition, tpe: Type) {
  override def toString: String = {
    val str = new StringBuilder
    str.append("\tlazy val ")
      .append(typeVariableName)
      .append(" = ")
      .append(typeVariableDefinition.toString)

    str.toString
  }
}

//For ObjectType of Query Type.
case class GQLObjectTypeDefinition(typeName: String, className:String, description: String, interfaces:String = "", fields: List[GQLFieldDefinition]) extends GQLTypeDefinition{
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
case class GQLFieldDefinition(fieldName: String, fieldType: String, wrappingFields:List[GQLDefaultGenericType.Value] = List(), fieldResolver:String) {
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

//For ObjectType of Mutation Type
case class GQLInputObjectTypeDefinition(typeName: String, className:String, description: String, interfaces:String = "", fields: List[GQLInputFieldDefinition]) extends GQLTypeDefinition{
  override def toString: String = {
    val str = new StringBuilder
    str.append("InputObjectType(")
      .append("\n")
      .append("\t\t\""+typeName +"\",")
      .append("\n")
      .append("\t\t\""+description+"\",")
      .append("\n")
      .append("\t\tList(")
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
case class GQLInputFieldDefinition(fieldName: String, fieldType: String, wrappingFields:List[GQLDefaultGenericType.Value] = List(), description:String) {
  override def toString: String = {
    //need to add wrapping types if any.
    //Note - while resolving option and type only scalar value is returned, so need to add wrapping type.
    //e.g. Option[List[String]] is resolved to scalar value String, and wrapping type is (list, Option).
    val wrappedType = wrappingFields.map(_.toString).foldLeft(fieldType){
      (a, b) => b +"(" + a + ")"
    }
    "InputField(\""+fieldName+"\", "+wrappedType+", description = \""+description+"\")"
  }
}

//For Enum Type for both query and mutation type.
case class GQLEnumTypeDefinition(name:String, description:String, values: List[GQLEnumFieldDefinition]) extends GQLTypeDefinition {
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
case class GQLEnumFieldDefinition(name: String, value:String, description:String) {
  override def toString: String = {
    //EnumValue("crm", value = Module.crm, description = Some("CRM module"))
    "EnumValue("+name+", value = "+value+", description = "+description+")"
  }
}

