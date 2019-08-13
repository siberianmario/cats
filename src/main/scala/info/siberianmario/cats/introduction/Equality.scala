package info.siberianmario.cats.introduction

import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.long._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._

object Equality extends App {

  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (date1, date2) =>
      date1.getTime === date2.getTime
    }

  val x = new Date()
  println(x === x)

  val y = new Date()
  println(x =!= y)

  println(123.some =!= none[Int])

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 =!= cat2)
  println(optionCat1 =!= optionCat2)
  println(cat1.some === cat1.some)
}
