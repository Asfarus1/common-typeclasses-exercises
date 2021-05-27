package fpcourse

import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import cats.laws.discipline.MonadErrorTests
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class GetSpec extends AnyFunSuite with Matchers with Configuration with FunSuiteDiscipline with Generators {
  test("getIntBE fails on insufficient input") {
    val bytes = List[Byte](3, 1, 2)
    Get.getIntBE.run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("getIntBE should read an int in big endian order") {
    val bytes = List[Byte](3, 1, 2, 1, 5, 3, 2)
    Get.getIntBE.run(bytes) shouldBe(Right(List[Byte](5, 3, 2), 50397697))
  }

  test("getIntLE fails on insufficient input") {
    val bytes = List[Byte](3, 1, 2)
    Get.getIntBE.run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("getIntLE should read an int in little endian order") {
    val bytes = List[Byte](3, 1, 2, 1, 5, 3, 2)
    Get.getIntLE.run(bytes) shouldBe(Right(List[Byte](5, 3, 2), 16908547))
  }

  test("getByte fails on insufficient input") {
    val bytes = List[Byte]()
    Get.getByte.run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("getByte should read a byte") {
    val bytes = List[Byte](4, 3, 2)
    Get.getByte.run(bytes) shouldBe(Right(List[Byte](3, 2), 4))
  }

  test("isEmpty should be true when input is fully consumed") {
    val bytes = List[Byte]()
    Get.isEmpty.run(bytes) shouldBe(Right((bytes, true)))
  }

  test("isEmpty should be false when there is input remaining") {
    val bytes = List[Byte](5)
    Get.isEmpty.run(bytes) shouldBe(Right((bytes, false)))
  }

  test("skip fails on insufficient input") {
    val bytes = List[Byte](5, 3)
    Get.skip(3).run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("skip consumes bytes and returns no result") {
    val bytes = List[Byte](5, 3, 8, 1)
    Get.skip(3).run(bytes) shouldBe(Right((List[Byte](1), ())))
  }

  test("getString fails on insufficient input") {
    val bytes = List[Byte](1, 6, 2, 1)
    Get.getString(5).run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("getString should read a string of N characters") {
    val bytes = List[Byte](104, 97, 112, 112, 121, 2, 1)
    Get.getString(5).run(bytes) shouldBe(Right((List[Byte](2, 1), "happy")))
  }

  /**
   * TODO 12
   * Write discipline tests for instances of Monoid, Eq and MonadError.
   */
  checkAll("Monoid[Get[Int]]", MonoidTests[Get[Int]].monoid)
  checkAll("Eq[Get[Int]]", EqTests[Get[Int]].eqv)
  checkAll("MonadError[Get, String]", MonadErrorTests[Get, String].monadError[Int, Int, Int])
}
