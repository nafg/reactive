package reactive

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
  /**
   * Supported logging levels, in order of most inclusive to most limited
   * (i.e., when setting the logging level, each level includes events
   * that are logged at levels to the right of it).
   * Do not assume levels ids won't change!
   */
  object Levels extends Enumeration {
    val Trace, Warn, Error, None = Value
  }

  private[reactive] val allES = new EventSource[(Levels.Value, Logger#LogEvent)] {}
  private val eventStreams = Levels.values.toList.map(level => (level -> allES.collect{ case (`level`, e) => e })).toMap

  /**
   * Fires all semantic events, as a level -> LogEvent tuple
   */
  def all: EventStream[(Levels.Value, Logger#LogEvent)] = allES
  /**
   * Fires all trace LogEvents
   */
  def traces: EventStream[Logger#LogEvent] = eventStreams(Levels.Trace)
  /**
   * Fires all warningLogEvents
   */
  def warnings: EventStream[Logger#LogEvent] = eventStreams(Levels.Warn)
  /**
   * Fires all error LogEvents
   */
  def errors: EventStream[Logger#LogEvent] = eventStreams(Levels.Error)

  /**
   * The log level that subsequently created Loggers should default to
   */
  var defaultLevel: Levels.Value = Levels.Warn
}

/**
 * This trait supplies semantic logging capabilities.
 * Logged events are fired from the corresponding
 * event streams in the Logger singleton
 */
trait Logger {
  @deprecated("No longer needed; predicates can be Any", "0.2")
  trait LogEventPredicate
  case class LogEvent(subject: AnyRef, predicate: Any)

  var logLevel: Logger.Levels.Value = Logger.defaultLevel
  /**
   * Sets the log level and returns the original instance. So for instance:
   * eventSource.setLogLevel(Logger.Levels.None).fire(mouseEvent)
   * val lessThanTen = eventStream.setLogLevel(Logger.Levels.Trace).filter(_ < 10)
   */
  def setLogLevel(level: Logger.Levels.Value): this.type = {
    logLevel = level
    this
  }

  /**
   * The subject parameter that LogEvents will be created with
   */
  def subject = this

  /**
   * Log a semantic event at the specified level
   */
  def log(level: Logger.Levels.Value, pred: => Any): Unit =
    if (logLevel != null && logLevel.id <= level.id)
      Logger.allES fire level -> LogEvent(subject, pred)

  /**
   * Log a semantic event at the trace level
   */
  def trace(pred: => Any) = log(Logger.Levels.Trace, pred)
  /**
   * Log a semantic event at the warning level
   */
  def warn(pred: => Any) = log(Logger.Levels.Warn, pred)
  /**
   * Log a semantic event at the error level
   */
  def error(pred: => Any) = log(Logger.Levels.Error, pred)
}
