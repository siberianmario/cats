package info.siberianmario.cats

trait Printable[A] {
  def format(a: A): String
}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

  def print[A](a: A)(implicit p: Printable[A]): Unit = println(format(a))
}

object PrintableInstances {
  implicit val printableInt: Printable[Int] = (i: Int) => i.toString
  implicit val printableString: Printable[String] = (i: String) => i
}