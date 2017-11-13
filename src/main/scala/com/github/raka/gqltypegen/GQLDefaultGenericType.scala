package com.github.raka.gqltypegen

/**
  * List and Option types in sangria-graphql - for Query type and Mutation Type.
  *
  * Created by Ranjit Kaliraj on 11/6/17.
  */
object GQLDefaultGenericType extends Enumeration {
  val LIST = Value("ListType")
  val OPTION = Value("OptionType")
  val INPUT_LIST = Value("InputListType")
  val INPUT_OPTION = Value("InputOptionType")
}
