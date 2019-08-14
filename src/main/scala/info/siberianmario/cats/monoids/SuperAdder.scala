package info.siberianmario.cats.monoids

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._
import cats.syntax.option._

case class Order(totalCost: Double, quantity: Double)

object SuperAdder extends App {

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = {
    items.foldLeft(monoid.empty)(_ |+| _)
  }

  val intList = List(1, 2, 3, 4)
  println(add(intList))

  val optIntList = List(1.some, none, 3.some, 4.some)
  println(add(optIntList))

  val orderList = List(Order(12, 5), Order(50, 10), Order(100, 2))
  println(add(orderList))
}
