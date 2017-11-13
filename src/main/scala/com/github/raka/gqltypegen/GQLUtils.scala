package com.github.raka.gqltypegen

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

/**
  * Created by Ranjit Kaliraj on 11/12/17.
  */
object GQLUtils {

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
