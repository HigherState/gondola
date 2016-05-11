package gondola.data

import org.scalatest.{Matchers, FunSpec}

class ExpirationMapTests extends FunSpec with Matchers {

  def testExpirationManager =
    new ExpiryManager[Long] {
      var getCurrent: Long = 0

      override def isExpired(value: Long, current: Long): Boolean =
        value < current

      override def age(value: Long, current: Long): Long =
        current - value

      override def newExpiry(): Long =
        getCurrent + 10
    }

  describe("Creating an empty ExpireMap") {
    describe("at the start ...") {
      it("... should be empty") {
        val eMap = ExpirationMap.empty[String, String, Long](testExpirationManager)
        eMap.isEmpty should be(true)
      }
    }
    describe("when add an element from empty") {
      it("the size should be 1") {
        val eMap = ExpirationMap.empty[String, String, Long](testExpirationManager)
        val append = eMap + ("first" -> "value")
        append.length should be(1)
      }
      it("adding (\"onekey\" -> \"onevalue\") I expect the value \"onevalue\" ") {
        val eMap = ExpirationMap.empty[String, String, Long](testExpirationManager)
        val append = eMap + ("onekey" -> "onevalue")
        append.get("onekey") should be(Some("onevalue"))
      }
      it("adding (\"onekey\" -> \"onevalue\") I expect to not find \"secondkey\" ") {
        val eMap = ExpirationMap.empty[String, String, Long](testExpirationManager)
        val append = eMap + ("onekey" -> "onevalue")
        append.get("secondkey") should be(None)
      }

      it("access expired value get a None") {
        val manager = testExpirationManager
        val eMap = ExpirationMap.empty[String, String, Long](manager)
        val append = eMap + ("onekey" -> "onevalue")
        manager.getCurrent = 50
        append.get("onekey") should be(None)
        append.length should be (0)
      }
    }
    describe("when add more than one element") {
      it("two element with same expiration could be accessible if accessed before expired") {
        val manager = testExpirationManager
        val eMap = ExpirationMap.empty[Int, String, Long](manager)
        val append = eMap ++ List((1 -> "one"),(2 -> "two"))
        manager.getCurrent = 2  // wait a bit
        append.length should be (2)
      }
      it("two element with different expire time could be accessible if accessed before expired") {
        val manager = testExpirationManager
        val eMap = ExpirationMap.empty[Int, String, Long](manager)
        val append = eMap + (1 -> "one")
        val append2 = append + (2 -> "two")
        manager.getCurrent = 3
        append2.length should be (2)
      }
      it("add 3 elements, expire 1, only two are available") {
        val manager = testExpirationManager
        val eMap = ExpirationMap.empty[Int, String, Long](manager)
        val append = eMap + (1 -> "one")
        manager.getCurrent = 6
        val append2 = append ++ List((2 -> "two"),(3 -> "three"))
        manager.getCurrent = 13
        append2.length should be (2)
      }
    }
  }

    //val res = ExpirationMap([10.toLong, 1.toLong] => this.testExpirationManager)


      //new ExpirationMap[0, 1, this.testExpirationManager]()
    //println ("class = " + rs.getClass)
    //rs.map(ExpirationMap.empty(_))
    // val res = aVect.toMap[Long, Boolean](this.testExpirationManager)

    //ImplicitMonadTest.errorValue[X, String](m.pure(4), "Not Odd") should equal (m.pure("Not Odd"))
}

