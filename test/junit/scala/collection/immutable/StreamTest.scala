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
  
  /** Test helper for Stream operations allowing GC during processing */
  def assertStreamOpAllowsGC[A, B](a: A, b: B, op: (=> Stream[A], A => B) => Any): Unit = {
    val msgSuccessGC = "GC success"
    val msgFailureGC = "GC failure"

    val ref = WeakReference( Stream.continually(a).take(500) )
    
    def gcAndThrowIfCollected(dummy: A): B = {
      System.gc()
      Thread.sleep(10)
      if (ref.get.isEmpty) throw new RuntimeException(msgSuccessGC)
      assertTrue(dummy == a) // protect from JIT optimizations
      b
    }

    val res = Try { op(ref(), gcAndThrowIfCollected) }
    val msg = res.failed.map(_.getMessage).getOrElse(msgFailureGC)
    
    assertTrue(msg == msgSuccessGC)
  }

  @Test
  def foreach_allows_GC() {
    assertStreamOpAllowsGC[Int, Unit](42, (), _.foreach(_))
  }

  @Test
  def filter_foreach_allows_GC() {
    assertStreamOpAllowsGC[Int, Unit](42, (), _.filter(_ => true).foreach(_))
  }

  @Test // SI-8990
  def withFilter_foreach_allowsGC: Unit = {
    assertStreamOpAllowsGC[Int, Unit](42, (), _.withFilter(_ => true).foreach(_))
  }

}
