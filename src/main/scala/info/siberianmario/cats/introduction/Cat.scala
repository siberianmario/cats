package info.siberianmario.cats.introduction

import cats.{Eq, Show}
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.show._
import info.siberianmario.cats.introduction.PrintableInstances._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val printableCat: Printable[Cat] = (cat: Cat) => {
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)
    s"$name is a $age year-old $color cat"
  }

  implicit val catEq: Eq[Cat] =
    Eq.instance { (cat1, cat2) =>
      cat1.name === cat2.name &&
        cat1.age === cat2.age &&
        cat1.color === cat2.color
    }

  implicit val catShow: Show[Cat] = Show.show { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat"
  }
}