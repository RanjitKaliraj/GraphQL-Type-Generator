package com.github.raka.gqltypegen

import scala.reflect.runtime.universe._

/**
  * Output Scalar Type must be - Int, Float, Boolean, String, Long, BigDecimal and BigInt.
  * Note giving custom value to Other nested Entity Type/case class or Enum type is not supported.
  *
  * Created by Ranjit Kaliraj on 11/6/17.
  *
  */
abstract class CustomScalarType[T: TypeTag,U: TypeTag] {

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
