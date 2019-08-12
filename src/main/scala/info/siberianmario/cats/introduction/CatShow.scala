package info.siberianmario.cats.introduction

import cats.syntax.show._

object CatShow extends App {
  val pussy = Cat("Tom", 6, "black")
  println(pussy.show)
}
