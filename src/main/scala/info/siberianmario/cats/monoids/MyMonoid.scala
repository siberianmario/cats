package info.siberianmario.cats.monoids

import info.siberianmario.cats.monoids.BoolInstances._
import info.siberianmario.cats.monoids.SetInstances._

trait MySemigroup[A] {
  def combine(x: A, y: A): A
}

object MySemigroup {
  def apply[A: MySemigroup]: MySemigroup[A] = implicitly[MySemigroup[A]]

  def associativeLaw[A: MySemigroup](x: A, y: A, z: A): Boolean = {
    MySemigroup[A].combine(x, MySemigroup[A].combine(y, z)) ==
        MySemigroup[A].combine(MySemigroup[A].combine(x, y), z)
  }
}

trait MyMonoid[A] extends MySemigroup[A] {
  def empty: A
}

object MyMonoid {
  def apply[A: MyMonoid]: MyMonoid[A] = implicitly[MyMonoid[A]]

  def identityLaw[A: MyMonoid](x: A): Boolean = {
    (MyMonoid[A].combine(x, MyMonoid[A].empty) == x) &&
        (MyMonoid[A].combine(MyMonoid[A].empty, x) == x)
  }
}

object BoolInstances {
  implicit val boolAndMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val boolOrMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val boolXorMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x ^ y
  }

  implicit val boolXnorMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = !(!x ^ !y)
  }
}

object SetInstances {
  implicit def unionSetMonoid[A]: MyMonoid[Set[A]] = new MyMonoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  implicit def diffSetMonoid[A]: MyMonoid[Set[A]] = new MyMonoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
  }

  implicit def intersectSetSemigroup[A]: MySemigroup[Set[A]] =
    (x: Set[A], y: Set[A]) => x intersect y
}

object MonoidApp extends App {
  def getCombinations3[T](values: Seq[T]): Seq[(T, T, T)] = for {
    x <- values
    y <- values
    z <- values
  } yield (x, y, z)

  def testIdentityLaw[T: MyMonoid](values: Seq[T]): Boolean =
    values.forall(MyMonoid.identityLaw(_))

  def testAssociativeLaw[T: MySemigroup](values: Seq[(T, T, T)]): Boolean =
    values.forall(v => MySemigroup.associativeLaw(v._1, v._2, v._3))

  val boolValues = Seq(false, true)
  val boolCombinations = getCombinations3(boolValues)

  println(s"Associative Law for BoolAndMonoid: ${testAssociativeLaw(boolCombinations)(boolAndMonoid)}")
  println(s"Identity Law for BoolAndMonoid: ${testIdentityLaw(boolValues)(boolAndMonoid)}")

  println(s"Associative Law for BoolOrMonoid: ${testAssociativeLaw(boolCombinations)(boolOrMonoid)}")
  println(s"Identity Law for BoolOrMonoid: ${testIdentityLaw(boolValues)(boolOrMonoid)}")

  println(s"Associative Law for BoolXorMonoid: ${testAssociativeLaw(boolCombinations)(boolXorMonoid)}")
  println(s"Identity Law for BoolXorMonoid: ${testIdentityLaw(boolValues)(boolXorMonoid)}")

  println(s"Associative Law for BoolXnorMonoid: ${testAssociativeLaw(boolCombinations)(boolXnorMonoid)}")
  println(s"Identity Law for BoolXnorMonoid: ${testIdentityLaw(boolValues)(boolXnorMonoid)}")

  println("---------------------------------")

  val setValues = Seq(Set(1, 2, 3), Set(2, 3, 4), Set(3, 4, 5))
  val setCombinations = getCombinations3(setValues)
  println(s"Associative Law for SetUnionMonoid: ${testAssociativeLaw(setCombinations)(unionSetMonoid)}")
  println(s"Identity Law for SetUnionMonoid: ${testIdentityLaw(setValues)(unionSetMonoid)}")

  println(s"Associative Law for SetDiffMonoid: ${testAssociativeLaw(setCombinations)(diffSetMonoid)}")
  println(s"Identity Law for SetDiffMonoid: ${testIdentityLaw(setValues)(diffSetMonoid)}")

  println(s"Associative Law for SetIntersectSemigroup: ${testAssociativeLaw(setCombinations)(intersectSetSemigroup)}")
}
