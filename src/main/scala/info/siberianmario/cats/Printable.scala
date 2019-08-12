package info.siberianmario.cats

import info.siberianmario.cats.PrintableInstances._
import info.siberianmario.cats.PrintableSyntax._

trait Printable[A] {
  def format(a: A): String
}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

  def print[A](a: A)(implicit p: Printable[A]): Unit = println(format(a))
}

object PrintableInstances {
  implicit val printableInt: Printable[Int] = (i: Int) => i.toString
  implicit val printableString: Printable[String] = (str: String) => str
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)

    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val printableCat: Printable[Cat] = (cat: Cat) => {
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)
    s"$name is a $age year-old $color cat"
  }
}

object PrintableApp extends App {
  val pussy = Cat("Tom", 6, "black")
  Printable.print(pussy)
  pussy.print
}