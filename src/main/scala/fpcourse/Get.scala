package fpcourse

import cats._
import cats.implicits._
import org.scalacheck.Gen

import java.nio.{ByteBuffer, ByteOrder}

/**
 * The Get monad parses values from a list of bytes, keeping track of the
 * remaining input after each operation.
 *
 * The run function reads and consumes the bytes needed to construct a value of A.
 * If there is any issue (i.e: insufficient input) it should signal it via a Left result.
 * If everything goes ok, it should return the remaining input along with the parsed value.
 *
 * For more information of a real implementation for Haskell, check out:
 * https://wiki.haskell.org/Dealing_with_binary_data
 */
case class Get[A](run: List[Byte] => Either[String, (List[Byte], A)])

object Get {
  /**
   * TODO 1
   * Consumes n bytes of input parsing no value.
   */
  def skip(n: Int): Get[Unit] = Get[Unit] {
    consume(n) fmap {
      case Left(msg) => msg.asLeft
      case Right((bytes, _)) => (bytes, ()).asRight
    }
  }

  def consume(n: Int): List[Byte] => Either[String, (List[Byte], Array[Byte])] = {
    bytes =>
      bytes.splitAt(n) match {
        case (read, tail) if read.length == n => (tail, read.toArray).asRight
        case _ => "Insufficient input".asLeft
      }
  }

  /**
   * TODO 2
   * True if the input is fully consumed
   */
  def isEmpty: Get[Boolean] = Get[Boolean] {
    bytes => (bytes, bytes.isEmpty).asRight
  }

  /**
   * TODO 3
   * Reads one byte from input
   */
  def getByte: Get[Byte] = Get[Byte] {
    consume(1) fmap {
      case Left(msg) => msg.asLeft
      case Right((bytes, read)) => (bytes, read.head).asRight
    }
  }

  /**
   * TODO 4
   * Reads an Int from input using Big Endian order.
   */
  def getIntBE: Get[Int] = Get[Int] {
    consume(4) fmap {
      case Left(msg) => msg.asLeft
      case Right((bytes, read)) => (bytes, bytesToIntUnsafe(read, ByteOrder.BIG_ENDIAN)).asRight
    }
  }

  /**
   * TODO 5
   * Reads an Int from input using Little Endian order.
   */
  def getIntLE: Get[Int] = Get[Int] {
    consume(4) fmap {
      case Left(msg) => msg.asLeft
      case Right((bytes, read)) => (bytes, bytesToIntUnsafe(read, ByteOrder.LITTLE_ENDIAN)).asRight
    }
  }

  /**
   * TODO 6
   * Reads a String of n characters from input.
   */
  def getString(n: Int): Get[String] = Get[String] {
    consume(n) fmap {
      case Left(msg) => msg.asLeft
      case Right((bytes, read)) => (bytes, new String(read)).asRight
    }
  }

  /**
   * Helper function that turns four bytes into an Int. It doesn't check the
   * length of the array, so please make sure to provide 4 bytes.
   */
  private def bytesToIntUnsafe(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }

  /**
   * TODO 7
   * Instance of monad error for Get.
   */
  implicit val monadGet: MonadError[Get, String] = new MonadError[Get, String] {
    override def flatMap[A, B](fa: Get[A])(f: A => Get[B]): Get[B] = Get[B] {
      bytes =>
        fa.run(bytes) match {
          case Left(msg) => msg.asLeft
          case Right(a) => f(a._2).run(bytes) match {
            case Left(msg) => msg.asLeft
            case Right(a1) => (bytes, a1._2).asRight
          }
        }
    }

    override def pure[A](x: A): Get[A] = Get(bytes => (bytes, x).asRight[String])

    override def tailRecM[A, B](a: A)(f: A => Get[Either[A, B]]): Get[B] = {
      Get { bytes =>
        Monad[Either[String, *]].tailRecM((bytes, a)) { case (bytes, a) =>
          f(a).run(bytes).map { case (bytes, eab) =>
            eab match {
              case Right(b) => Right((bytes, b))
              case Left(a) => Left((bytes, a))
            }
          }
        }
      }
    }

    override def raiseError[A](e: String): Get[A] = Get[A] {
      _ => e.asLeft[(List[Byte], A)]
    }

    override def handleErrorWith[A](fa: Get[A])(f: String => Get[A]): Get[A] = Get[A] {
      bytes =>
        fa.run(bytes) match {
          case Left(e) => f(e).run(bytes) match {
            case Left(msg) => msg.asLeft
            case Right(b) => (bytes, b._2).asRight[String]
          }
          case r => r
        }
    }

  }

  /**
   * TODO 8
   * Instance of Eq for Get. A full comparison is impossible, so we just
   * compare on a given number of List[Byte] samples and assume that
   * if both Get compute the same result, they are equal.
   *
   * Hint: One possible way of doing this is to use scalacheck to build
   * a generator of List[Byte], then sample it several times (e.g. 32)
   * and check that running both Gets yields the same result every time.
   */
  implicit def eqGet[A: Eq]: Eq[Get[A]] = Eq.instance {
    (a, b) =>
      val bytegen = Gen.listOf[Byte](Gen.choose[Int](0, 255).map(_.toByte))

      (1 to 32).forall(_ => {
        val bytes = bytegen.sample.get
        a.run(bytes) === b.run(bytes)
      })
  }

  /**
   * TODO 9
   * Monoid instance for Get.
   */
  implicit def monoid[A: Monoid]: Monoid[Get[A]] = new Monoid[Get[A]] {
    override def empty: Get[A] = Monad[Get].pure(Monoid[A].empty)

    override def combine(x: Get[A], y: Get[A]): Get[A] = for {
      a <- x
      b <- y
    } yield a |+| b
  }

}