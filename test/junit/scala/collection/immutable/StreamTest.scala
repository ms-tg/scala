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

  private def gcAndThrowIfCollected(ref: WeakReference[Stream[Int]])(d: Any): Unit = {
    System.gc()
    Thread.sleep(100)
    if (ref.get.isEmpty) throw new RuntimeException("GC succeeded")
  }

  @Test
  def foreach_allows_GC() {
    val ref = WeakReference( Stream.continually(42).take(300) )
    Try { ref().foreach(gcAndThrowIfCollected(ref)) }
    assert( ref.get.isEmpty )
  }

  @Test // SI-8990
  def withFilter_foreach_allowsGC {
    val ref = WeakReference( Stream.continually(42).take(300) )
    Try { ref().withFilter(_ => true).foreach(gcAndThrowIfCollected(ref)) }
    assert( ref.get.isEmpty )
  }

}
