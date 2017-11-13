package com.github.raka.gqltypegen

/**
  * Created by Ranjit Kaliraj on 11/8/17.
  */
object StringUtil {

  def camelcase(s: String): String ={
    if(s.head == '_'){
      "_".concat((s.tail.split("_").toList match {
        case head :: tail => head :: tail.map(_.capitalize)
        case x => x
      }).mkString)
    }
    else{
      (s.split("_").toList match {
        case head :: tail => head :: tail.map(_.capitalize)
        case x => x
      }).mkString
    }
  }

  def snakecase(s: String): String = s.foldLeft(new StringBuilder) {
    case (s, c) if Character.isUpperCase(c) =>
      s.append("_").append(Character.toLowerCase(c))
    case (s, c) =>
      s.append(c)
  }.toString

}
