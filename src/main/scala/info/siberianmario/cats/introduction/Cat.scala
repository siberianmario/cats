package info.siberianmario.cats.introduction

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import info.siberianmario.cats.introduction.PrintableInstances._
import info.siberianmario.cats.introduction.PrintableSyntax._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val printableCat: Printable[Cat] = (cat: Cat) => {
    val name = cat.name.formats
    val age = cat.age.formats
    val color = cat.color.formats
    s"$name is a $age year-old $color cat"
  }

  implicit val catShow: Show[Cat] = Show.show { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat"
  }
}