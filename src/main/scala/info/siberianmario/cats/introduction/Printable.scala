package info.siberianmario.cats.introduction

import info.siberianmario.cats.introduction.PrintableSyntax._

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  implicit val printableInt: Printable[Int] = (i: Int) => i.toString
  implicit val printableString: Printable[String] = (str: String) => str
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def formats(implicit p: Printable[A]): String = p.format(value)

    def print(implicit p: Printable[A]): Unit = println(formats(p))
  }
}

object PrintableApp extends App {
  val pussy = Cat("Tom", 6, "black")
  pussy.print
}