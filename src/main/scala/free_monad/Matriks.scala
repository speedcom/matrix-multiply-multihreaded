package free_monad

import normal.{OneThreadStrategy, ThreadStrategy}

import scalaz.Free._
import scalaz._

sealed trait Matriks[+Next]
case class Row[Next](row: Seq[Int], next: Next) extends Matriks[Next]

object MatriksImplicits {

  implicit def functorMatriks = new Functor[Matriks] {
    override def map[A, B](fa: Matriks[A])(f: (A) => B): Matriks[B] = fa match {
      case Row(row, next) => Row(row, f(next))
    }
  }

  type FreeMatriks[A] = Free[Matriks, A]

  def row(row: Seq[Int]): FreeMatriks[Unit] = liftF(Row(row, ()))

  def matrix: FreeMatriks[Unit] = for {
    _ <- row(Seq(1,1,1,1))
    _ <- row(Seq(1,1,1,1))
    _ <- row(Seq(1,1,1,1))
    _ <- row(Seq(1,1,1,1))
    _ <- row(Seq(1,1,1,1))
    _ <- row(Seq(1,1,1,1))
    _ <- row(Seq(1,1,1,1))
    _ <- row(Seq(1,1,1,1))
  } yield ()

  def matrix_2: FreeMatriks[Unit] = for {
    _ <- row(Seq(2,3,4,5,6,7,8,9))
    _ <- row(Seq(2,3,4,5,6,7,8,9))
    _ <- row(Seq(2,3,4,5,6,7,8,9))
    _ <- row(Seq(2,3,4,5,6,7,8,9))
  } yield ()

}

// INTERPRETERS
object A extends App {
  import MatriksImplicits._

  def console[A](freeM: FreeMatriks[A]): Unit = freeM.resume.fold({
    case Row(row, next) => println(row); console(next)
  }, _ => ())

  console(MatriksImplicits.matrix)
  console(MatriksImplicits.matrix_2)

  def col(i: Int, matrix: FreeMatriks[Unit], result: List[Int] = List()): List[Int] = matrix.resume.fold({
    case Row(row, next) => col(i, next, row(i) :: result)
  }, _ => result)

  implicit val threading: ThreadStrategy = OneThreadStrategy

  def multiply(l: FreeMatriks[Unit], r: FreeMatriks[Unit])(implicit threading: ThreadStrategy): Unit = (l, r) match {
    case (Row(lRow, lNext), Row(rRow, rNext)) => threading.execute { () =>

      val cols = for {
        i <- (0 until rRow.size)
      } yield col(i, r)

      println(s"cols: $cols")

    }
  }

}