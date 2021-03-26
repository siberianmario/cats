package info.siberianmario.cats.introduction

import info.siberianmario.cats.introduction.PrintableInstances._
import info.siberianmario.cats.introduction.PrintableSyntax._

trait Printable[-A] {
  def format(a: A): String
}

object Printable {
  def apply[A: Printable]: Printable[A] = implicitly[Printable[A]]
  
  def format[A: Printable](a: A): String = Printable[A].format(a)

  def print[A: Printable](a: A): Unit = println(format(a))
}

object PrintableInstances {
  implicit val printableInt: Printable[Int] = (i: Int) => i.toString
  implicit val printableString: Printable[String] = (str: String) => str
  implicit def printableOption[T: Printable]: Printable[Option[T]] = new Printable[Option[T]] {
    override def format(option: Option[T]): String = option match {
      case Some(value) => Printable[T].format(value)
      case None => "empty Option"
    }
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A: Printable](value: A) {
    def format: String = Printable.format(value)

    def print: Unit = Printable.print(value)
  }
}

object PrintableApp extends App {
  val pussy = Cat("Tom", 6, "black")
  pussy.print
  Option(pussy).print
  Some(pussy).print
}