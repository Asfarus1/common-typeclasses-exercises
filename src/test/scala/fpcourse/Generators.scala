package fpcourse

import org.scalacheck.{Arbitrary, Gen}
import cats.implicits._


trait Generators {
  /**
   * TODO 10
   * Instance of Arbitrary for functions A => A.
   * No constraints on what the function does.
   * Simple solutions should be enough.
   */
  implicit def arbFun[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] = {
    val funGen = for {
      v2 <- arbA.arbitrary
      b <- Gen.oneOf(true, false)
    } yield (v1: A) => v1//if (b) v1 else v2
    Arbitrary(funGen)
  }

  /**
   * TODO 11
   * Instance of Arbitrary for the Get monad.
   * Any solution should suffice.
   * Some optional constraints if you want to spice things up:
   * - allow for both successful and failed return values to be produced
   * - when running the Get successfully, the remaining bytes should be
   * a suffix of the bytes passed as argument to the run function; that is,
   * you should simulate actual 'consumption' of the bytes.
   */
  implicit def arbGet[A](implicit arbA: Arbitrary[A]): Arbitrary[Get[A]] = {
    val funGen = for {
      v2 <- arbA.arbitrary
      len <- Gen.posNum[Int]
    } yield Get[A]{
      Get.consume(len) fmap {
        case Left(msg) => msg.asLeft[(List[Byte], A)]
        case Right((bytes, _)) => (bytes, v2).asRight[String]
      }
    }
    Arbitrary(funGen)
  }
}
