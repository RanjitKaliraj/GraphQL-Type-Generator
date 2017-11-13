package com.github.raka.gqltypegen

/**
  * Created by Ranjit Kaliraj on 11/6/17.
  */

/**
  * Throw this exception if a field type is not a valid scalar type or
  * custom scalar type conversion is not defined for it. For example, DateTime, JsValue is not valid scalar type unless
  * you provide custom scalar conversion. Currently supported scalar type is - Boolean, Byte, Short, Int, Long, Float,
  * Double, Char, String. You must provide custom scalar type for other types except case class / entity type.
  */
class InvalidScalarTypeException(scalarType: String, msg: String) extends RuntimeException("Field: "+scalarType+", Reason: "+msg)

/**
  * Throw this exception in case client request to convert invalid entity object into GraphQL Type.
  * e.g. String, Number etc. as valid AnyRef but is not valid entity class. In other word, a class must be a case class
  * to be valid.
  */
class InvalidClassTypeException(msg: String) extends RuntimeException(msg)