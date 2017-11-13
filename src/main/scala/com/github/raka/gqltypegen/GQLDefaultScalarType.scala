package com.github.raka.gqltypegen

/**
  * Default GraphQL scalar types. Names are according to GQL specification for scalar types.
  * Builtin GraphQL scalar type = IntType :: FloatType :: BooleanType :: StringType
  * Buildin Sangria scalar type = LongType :: BigIntType :: BigDecimalType
  * todo - Add IDtype
  *
  * Created by Ranjit Kaliraj on 11/6/17.
  *
  */
object GQLDefaultScalarType extends Enumeration {
  val INT = Value("IntType")
  val FLOAT = Value("FloatType")
  val BOOLEAN = Value("BooleanType")
  val STRING = Value("StringType")
  val LONG = Value("LongType")
  val BIGDECIMAL = Value("BigDecimalType")
  val BIGINT = Value("BigIntType")
}
