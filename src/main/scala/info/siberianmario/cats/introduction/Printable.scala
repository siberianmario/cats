package info.siberianmario.cats.introduction

import info.siberianmario.cats.introduction.PrintableSyntax._
import info.siberianmario.cats.introduction.PrintableInstances._

trait Printable[-A] {
  def format(a: A): String
}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

  def print[A](a: A)(implicit p: Printable[A]): Unit = println(format(a))
}

object PrintableInstances {
  implicit val printableInt: Printable[Int] = (i: Int) => i.toString
  implicit val printableString: Printable[String] = (str: String) => str
  implicit def printableOption[T](implicit p: Printable[T]): Printable[Option[T]] = new Printable[Option[T]] {
    override def format(option: Option[T]): String = option match {
      case Some(value) => p.format(value)
      case None => "empty Option"
    }
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)

    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

object PrintableApp extends App {
  val pussy = Cat("Tom", 6, "black")
  pussy.print
  Option(pussy).print
  Some(pussy).print
}