package reactive
package logging

case class LogEvent(subject: AnyRef, predicate: Any)

class LogLevel(val threshold: Int)

object LogLevel {
  case object Trace extends LogLevel(50)
  case object Debug extends LogLevel(40)
  case object Info  extends LogLevel(30)
  case object Warn  extends LogLevel(20)
  case object Error extends LogLevel(10)
  case object Off   extends LogLevel(0)
}

/**
 * This singleton manages reactive's semantic logging.
 * To configure logging, forward (or foreach) events to your logging backend.
 * Examples:
 * Logger.traces =>> myLogger.trace
 * Logger.warnings.map(_.toString) =>> myLogger.warn(_)
 * for(e <- Logger.errors) System.err.println(e)
 * Logger.all foreach println
 * Make sure you're in the right Observing scope!
 */
object Logger {
  private[reactive] val allES = new EventSource[(LogLevel, LogEvent)] {}
  private val eventStreams = scala.collection.mutable.Map.empty[LogLevel, EventStream[LogEvent]]
  def events(level: LogLevel) = eventStreams.synchronized {
    eventStreams.getOrElseUpdate(level, allES.collect { case (`level`, e) => e })
  }

  /**
   * Fires all semantic events, as a level -> LogEvent tuple
   */
  lazy val all: EventStream[(LogLevel, LogEvent)] = allES
  /**
   * Fires all trace LogEvents
   */
  lazy val traces: EventStream[LogEvent] = events(LogLevel.Trace)
  /**
   * Fires all warning LogEvents
   */
  lazy val warnings: EventStream[LogEvent] = events(LogLevel.Warn)
  /**
   * Fires all error LogEvents
   */
  lazy val errors: EventStream[LogEvent] = events(LogLevel.Error)

  /**
   * The log level that subsequently created Loggers should default to
   */
  var defaultLevel: LogLevel = LogLevel.Warn
}

/**
 * This trait supplies semantic logging capabilities.
 * Logged events are fired from the corresponding
 * event streams in the Logger singleton
 */
trait Logger {
  @deprecated("Use reactive.logging.LogEvent", "0.4.0")
  type LogEvent = reactive.logging.LogEvent
  @deprecated("Use reactive.logging.LogEvent", "0.4.0")
  def LogEvent = reactive.logging.LogEvent

  var logLevel: LogLevel = Logger.defaultLevel

  /**
   * Sets the log level and returns the original instance. So for instance:
   * eventSource.setLogLevel(Logger.Levels.None).fire(mouseEvent)
   * val lessThanTen = eventStream.setLogLevel(Logger.Levels.Trace).filter(_ < 10)
   */
  def setLogLevel(level: LogLevel): this.type = {
    logLevel = level
    this
  }

  /**
   * The subject parameter that LogEvents will be created with
   */
  def subject: AnyRef = this

  /**
   * Log a semantic event at the specified level
   */
  def log(level: LogLevel, pred: => Any): Unit =
    if (logLevel != null && logLevel.threshold >= level.threshold)
      Logger.allES fire level -> reactive.logging.LogEvent(subject, pred)

  /**
   * Log a semantic event at the trace level
   */
  def trace(pred: => Any) = log(LogLevel.Trace, pred)
  /**
   * Log a semantic event at the warning level
   */
  def warn(pred: => Any) = log(LogLevel.Warn, pred)
  /**
   * Log a semantic event at the error level
   */
  def error(pred: => Any) = log(LogLevel.Error, pred)
}

trait HasLogger { outer =>
  object logger extends Logger {
    override def subject = outer
  }
}
