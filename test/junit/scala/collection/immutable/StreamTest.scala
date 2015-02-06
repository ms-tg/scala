package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.ref.WeakReference
import scala.util.Try

@RunWith(classOf[JUnit4])
class StreamTest {

  @Test
  def t6727_and_t6440(): Unit = {
    assertTrue(Stream.continually(()).filter(_ => true).take(2) == Seq((), ()))
    assertTrue(Stream.continually(()).filterNot(_ => false).take(2) == Seq((), ()))
    assertTrue(Stream(1,2,3,4,5).filter(_ < 4) == Seq(1,2,3))
    assertTrue(Stream(1,2,3,4,5).filterNot(_ > 4) == Seq(1,2,3,4))
  }

  /** Test helper to verify that the given Stream operation allows
    * GC of the head during processing of the tail.
    */
  def assertStreamOpAllowsGC[B](op: (=> Stream[Int], Int => B) => Any, f: Int => B): Unit = {
    val msgSuccessGC = "GC success"
    val msgFailureGC = "GC failure"

    val ref = WeakReference( Stream.from(1).take(500) ) // wait about 5 seconds to fail

    def gcAndThrowIfCollected(n: Int): B = {
      System.gc()
      Thread.sleep(10)
      if (ref.get.isEmpty) throw new RuntimeException(msgSuccessGC)
      assertTrue(n >= 1) // protect from JIT optimizations
      f(n)
    }

    val res = Try { op(ref(), gcAndThrowIfCollected) }
    val msg = res.failed.map(_.getMessage).getOrElse(msgFailureGC)

    assertTrue(msg == msgSuccessGC)
  }

  @Test
  def foreach_allows_GC() {
    assertStreamOpAllowsGC[Unit](_.foreach(_), _ => ())
  }

  @Test
  def filter_all_foreach_allows_GC() {
    assertStreamOpAllowsGC[Unit](_.filter(_ => true).foreach(_), _ => ())
  }

  @Test // SI-8990
  def withFilter_after_first_foreach_allows_GC: Unit = {
    assertStreamOpAllowsGC[Unit](_.withFilter(_ > 1).foreach(_), _ => ())
  }

  @Test // SI-8990
  def withFilter_after_first_withFilter_foreach_allows_GC: Unit = {
    assertStreamOpAllowsGC[Unit](_.withFilter(_ > 1).withFilter(_ < 100).foreach(_), _ => ())
  }

  @Test // SI-8990
  def withFilter_can_retry_after_exception_thrown_in_filter: Unit = {
    var shouldThrow = true
    def greaterThanFiveButMayThrow(n: Int) = if (shouldThrow && n == 5) throw new RuntimeException("n == 5") else n > 5
    val wf = Stream.from(1).take(10).withFilter(greaterThanFiveButMayThrow)
    Try { wf.map(identity) }                   // throws on n == 5
    shouldThrow = false
    assertTrue( wf.map(identity).length == 5 ) // success instead of NPE
  }

}
