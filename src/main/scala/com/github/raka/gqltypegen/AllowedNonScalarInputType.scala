package com.github.raka.gqltypegen

import scala.reflect.runtime.universe._

/**
  * Allowed non scalar type for Generating Query and Mutation types.
  * AllowedTo - non-scalar type to which is allowed without custom definition.
  * AllowedAs - AllowedTo type is considered as this type in GQL.
  * Note: you must be sure that GQL support implicit conversion from AllowedTo to AllowedAs type, otherwise, type will not work.
  *
  * Created by Ranjit Kaliraj on 11/6/17.
  *
  */
class AllowedNonScalarInputType[T: TypeTag, U : TypeTag] {
  val allowedTo: Type = typeOf[T]
  val allowedAs: Type = typeOf[U]
}
