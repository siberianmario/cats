package info.siberianmario.cats

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

import scala.collection.BuildFrom
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

object BatchedTraverse {
  def batchedTraverse[A, B, M[X] <: Iterable[X]](in: M[A], batchSize: Int)(
      f: A => Future[B])(implicit
      bf: BuildFrom[M[A], B, M[B]],
      ec: ExecutionContext): Future[M[B]] =
    in.grouped(batchSize)
      .foldLeft(Future.successful(bf.newBuilder(in))) { (bldrF, batch) =>
        for {
          bldr <- bldrF
          bs <- Future.traverse(batch)(f)
        } yield bldr.addAll(bs)
      }
      .map(_.result())
}

object TraverseApp extends App {

  import BatchedTraverse.batchedTraverse
  import scala.concurrent.ExecutionContext.Implicits.global

  val ids = 1 to 10000

  def printId(id: Int): Future[String] = Future {
    blocking {
      Thread.sleep(10)
      println(id)
      id.toString
    }
  }

  def compute(): Future[Unit] =
    // Future.traverse(ids)(printId).map { strs =>
    batchedTraverse(ids, 10)(printId).map { strs =>
      println(s"collection size is ${strs.size}")
    }

  Await.result(compute, Duration.Inf)
}
