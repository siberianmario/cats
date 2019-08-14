package info.siberianmario.cats.monoids

import info.siberianmario.cats.monoids.BoolInstances._
import info.siberianmario.cats.monoids.SetInstances._

trait MySemigroup[A] {
  def combine(x: A, y: A): A
}

object MySemigroup {
  def associativeLaw[A](x: A, y: A, z: A)
    (implicit s: MySemigroup[A]): Boolean = {
    s.combine(x, s.combine(y, z)) ==
      s.combine(s.combine(x, y), z)
  }
}

trait MyMonoid[A] extends MySemigroup[A] {
  def empty: A
}

object MyMonoid {
  def apply[A](implicit monoid: MyMonoid[A]) =
    monoid

  def identityLaw[A](x: A)
    (implicit m: MyMonoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
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
  implicit def setUnionMonoid[A]: MyMonoid[Set[A]] = new MyMonoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  implicit def setDiffMonoid[A]: MyMonoid[Set[A]] = new MyMonoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
  }

  implicit def setIntersectSemigroup[A]: MySemigroup[Set[A]] =
    (x: Set[A], y: Set[A]) => x intersect y
}

object MonoidApp extends App {
  def getCombinations3[T](values: Seq[T]): Seq[(T, T, T)] = for {
    x <- values
    y <- values
    z <- values
  } yield (x, y, z)

  def testIdentityLaw[T](values: Seq[T])(implicit m: MyMonoid[T]): Boolean =
  values.forall(MyMonoid.identityLaw(_)(m))

  def testAssociativeLaw[T](values: Seq[(T, T, T)])(implicit m: MySemigroup[T]): Boolean =
  values.forall(v => MySemigroup.associativeLaw(v._1, v._2, v._3)(m))

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
  println(s"Associative Law for SetUnionMonoid: ${testAssociativeLaw(setCombinations)(setUnionMonoid)}")
  println(s"Identity Law for SetUnionMonoid: ${testIdentityLaw(setValues)(setUnionMonoid)}")

  println(s"Associative Law for SetDiffMonoid: ${testAssociativeLaw(setCombinations)(setDiffMonoid)}")
  println(s"Identity Law for SetDiffMonoid: ${testIdentityLaw(setValues)(setDiffMonoid)}")

  println(s"Associative Law for SetIntersectSemigroup: ${testAssociativeLaw(setCombinations)(setIntersectSemigroup)}")
}
