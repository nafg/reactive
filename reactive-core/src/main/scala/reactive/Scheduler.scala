package reactive

import scala.concurrent.ExecutionContext

class Scheduler(implicit executionContext: ExecutionContext) {
  def schedule(work: =>Unit): Subscription = {
    @volatile var unsubscribed = false
    val subscription = new SimpleSubscription {
      def cleanUp = unsubscribed = true
    }
    executionContext execute new Runnable {
      def run = if(!unsubscribed) work
    }
    subscription
  }

  def schedule(work: (()=>Unit) => Unit): Subscription = {
    var subscription: Subscription = null
    def loop(): Unit = subscription = schedule { work(loop) }
    loop()
    subscription
  }
}
