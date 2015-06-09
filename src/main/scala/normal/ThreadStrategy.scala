package normal

import java.util.concurrent.{Callable, Executors}

trait ThreadStrategy {
  def execute[A](func: Function0[A]): Function0[A]
}

object OneThreadStrategy extends ThreadStrategy {
  override def execute[A](func: Function0[A]): Function0[A] = func
}

object ThreadPoolStrategy extends ThreadStrategy {

  val pool = Executors.newFixedThreadPool(java.lang.Runtime.getRuntime.availableProcessors())

  override def execute[A](func: Function0[A]): Function0[A] = {
    val future = pool.submit(new Callable[A] {
      override def call(): A = {
        Console.println("Wykonanie funkcji w watku: " + Thread.currentThread().getName)
        func()
      }
    })

    () => future.get()
  }
}